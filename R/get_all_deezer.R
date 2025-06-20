#'Get \emph{Deezer} Track Information
#'
#'Retrieve a data frame containing information from the \emph{Deezer} API.
#'The result contains information on tracks, albums, and artists as inferred by the track search. Threfore, only information on the percentage of linked tracks will be considered.
#'The \pkg{spotilink} naming convention is used.
#'
#' @param input
#'Data Frame containing the following columns:
#'\itemize{
#'  \item\code{track.s.id} \cr
#'  with \emph{Spotify} track ids,
#'  \item \code{track.s.title} \cr
#'  with \emph{Spotify} track name,
#'  \item \code{track.s.firstartist.name} \cr
#'  with \code{Spotify} name of first artist,
#'  \item \code{album.s.title} \cr
#'  with album title from \emph{Spotify}
#'}
#'It is advisable to first run \code{\link{get_tracks_spotify}} before running this command,
#'in order to have all the necessary information.
#'
#'@param track_threshold,album_threshold,firstartist_threshold
#'Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
#'The values correspond to the string similarity (1 - Jaro-Winkler distance) between the track / album title or artist name  on \emph{Spotify} and the found track / album title or artist name on \emph{Deezer}.
#'
#'@param artisttoptracks
#'TRUE or FALSE indicator wether to also collect the artists' top track. ATTENTION: This produces very large files and is not encouraged for larger datasets due to possible troubles when loading results, as well as reading and writing checkpoints.
#'
#' @return Data Frame with added information from the \emph{Deezer} API using the  \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
get_all_deezer <- function(input, track_threshold = 0.8, album_threshold = 0.8, firstartist_threshold = 0.8, artisttoptracks = T){
  are_needed_columns_present(input, c('track.s.id', 'track.s.title', 'track.s.firstartist.name', 'album.s.title'))
  input <- rename_existing_variables(input, c(deezerTrackVars))
  input_distinct <- dplyr::distinct(input, track.s.id, track.s.firstartist.name, album.s.title, .keep_all = T) %>% dplyr::filter(! is.na(track.s.id))


  deezer_tracks <- search_tracks_deezer(input_distinct)
  deezer_artists <- lookup_firstartists_deezer(deezer_tracks, artisttoptracks)
  deezer_albums <- lookup_trackalbums_deezer(deezer_tracks)

  message('Done.')

  result <- join_deezer_track_information(deezer_tracks,
                                          deezer_artists,
                                          deezer_albums)

  result <- filter_quality_deezer_all(result, track_threshold, album_threshold, firstartist_threshold, artisttoptracks)
  result <- suppressMessages(dplyr::left_join(input, result))

  print_linkage_for_id('track.dz.id', result)
  saveRDS(result, 'deezer_all.rds')
  deezer_remove_checkpoints()
  result
}

search_tracks_deezer <- function(input_distinct){
  checkpoint_name <- 'deezer_all_tracks'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  if(! last_index == nrow(input_distinct)){
    saved_data <- checkpoint$saved_data
    if(last_index > 0) {input_distinct <- tail(input_distinct, -last_index)}
    purrr::pmap_df(list(input_distinct$track.s.title,
                                         input_distinct$track.s.firstartist.name,
                                         input_distinct$track.s.id,
                                         input_distinct$album.s.title),
                                    get_single_track_deezer %>% save_checkpoint_and_count(checkpoint_name, last_index, saved_data,
                                                                                          savingstep = 100,
                                                                                          ndatapoints = nrow(input_distinct)),
                                    .progress = 'Retrieving tracks from Deezer...')
  }
  else{message('Tracks already linked.')}
  suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
}

lookup_firstartists_deezer <- function(deezer_tracks, artisttoptracks){
  unique_artist_urls <- deezer_tracks %>%
    dplyr::mutate(artist_urls = purrr::map_chr(track.dz.firstartist.id, create_dz_artist_lookup_url),
                  artist_urls = ifelse(artist_urls == 'https://api.deezer.com/artist/NA', NA, artist_urls)) %>%
    dplyr::filter(!is.na(artist_urls)) %>%
    dplyr::distinct(artist_urls)
  checkpoint_name <- 'deezer_all_artists'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  if(! last_index == nrow(unique_artist_urls)){
    saved_data <- checkpoint$saved_data
    artists <- if(last_index > 0) {unique_artist_urls <- tail(unique_artist_urls, -last_index)}
    purrr::map_df(unique_artist_urls$artist_urls,
                   lookup_single_firstartist_deezer %>% save_checkpoint_and_count(checkpoint_name, last_index, saved_data,
                                                                                  savingstep = 100,
                                                                                  ndatapoints = nrow(unique_artist_urls)),
                  artisttoptracks,
                   .progress = 'Looking up corresponding artists...')
  }
  else{message('Artist already linked.')}
  res <- suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
  suppressMessages(dplyr::left_join(deezer_tracks, res))
}

lookup_single_firstartist_deezer <- function(url, artisttoptracks){
  repeat{
    artist_lookup <- get_api_with_connection_management(url)
    if(is.null(artist_lookup$error)){
      artist <- parse_dz_artist_lookup(artist_lookup, NA)
      break
    } else if (artist_lookup$error$code == 800) { # no data
      artist <- make_na_frame_deezer_artists(NA)
      break
    } else {
      message('Quota limit reached. Waiting for 5 seconds...')
      Sys.sleep(5)
    }
  }
  res <- dplyr::select(artist,
                track.dz.firstartist.id = artist.dz.id,
                track.dz.firstartist.name = artist.dz.name,
                track.dz.firstartist.followers = artist.dz.followers,
                track.dz.firstartist.nalbums = artist.dz.nalbums,
                track.dz.firstartist.toptracks = artist.dz.toptracks)
  if(! artisttoptracks) {res <- dplyr::select(res, -track.dz.firstartist.toptracks)}
  res
}


lookup_trackalbums_deezer <- function(deezer_tracks){
  unique_album_urls <- deezer_tracks %>%
    dplyr::mutate(album_urls = purrr::map_chr(track.dz.album.id, create_dz_album_lookup_url),
                  album_urls = ifelse(album_urls == 'https://api.deezer.com/album/NA', NA, album_urls)) %>%
    dplyr::filter(!is.na(album_urls)) %>%
    dplyr::distinct(album_urls)
  checkpoint_name <- 'deezer_all_albums'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  if(! last_index == nrow(unique_album_urls)){
    saved_data <- checkpoint$saved_data
    if(last_index > 0) {unique_album_urls <- tail(unique_album_urls, -last_index)}
    purrr::map_df(unique_album_urls$album_urls,
                   lookup_single_trackalbum_deezer %>% save_checkpoint_and_count(checkpoint_name, last_index, saved_data,
                                                                                 savingstep = 100,
                                                                                 ndatapoints = nrow(unique_album_urls)),
                   .progress = 'Looking up corresponding albums...')
  }
  else{message('Albums already linked.')}
  res <- suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
  suppressMessages(dplyr::left_join(deezer_tracks, res))
}

lookup_single_trackalbum_deezer <- function(url){
  repeat{
    album_lookup <- get_api_with_connection_management(url)
    if(is.null(album_lookup$error)){
      album <- parse_dz_album_lookup(album_lookup, NA)
      break
    } else if (album_lookup$error$code == 800) { # no data
      album <- make_na_frame_deezer_albums(NA)
      break
    } else {
      message('Quota limit reached. Waiting for 5 seconds...')
      Sys.sleep(5)
    }
  }
  dplyr::select(album,
                track.dz.album.id = album.dz.id,
                track.dz.album.title = album.dz.title,
                track.dz.album.upc = album.dz.upc,
                track.dz.album.totaltracks = album.dz.totaltracks,
                track.dz.album.duration = album.dz.duration,
                track.dz.album.followers = album.dz.followers,
                track.dz.album.releasedate = album.dz.releasedate,
                track.dz.album.type = album.dz.type,
                track.dz.album.explicitlyrics = album.dz.explicitlyrics,
                track.dz.album.explicitlyricsinfo = album.dz.explicitlyricsinfo,
                track.dz.album.explicitcoverinfo = album.dz.explicitcoverinfo,
                track.dz.album.label = album.dz.label,
                track.dz.album.genres = album.dz.genres,
                track.dz.album.firstgenre.id = album.dz.firstgenre.id,
                track.dz.album.firstgenre.name = album.dz.firstgenre.name)
}

deezer_remove_checkpoints <- function(){
  pattern <- paste0('^deezer_all_(albums|tracks|artists)_(\\d+)\\.rds$')
  files <- list.files()
  matching_files <- grep(pattern, files, value = TRUE)
  for(file in matching_files){file.remove(file)}
}

join_deezer_track_information <- function(deezer_tracks, deezer_artists, deezer_albums){
  deezer_tracks_prejoin <- deezer_tracks %>% dplyr::filter(!is.na(track.dz.id)) %>%
    dplyr::select(-track.dz.firstartist.name, -track.dz.album.title) %>%
    dplyr::distinct(track.s.id, .keep_all = TRUE)
  deezer_artists_prejoin <- deezer_artists %>% dplyr::filter(!is.na(track.dz.firstartist.id)) %>%
    dplyr::distinct(track.dz.firstartist.id, .keep_all = TRUE) %>%
    dplyr::select(dplyr::contains('track.dz.firstartist'))
  deezer_albums_prejoin <-deezer_albums %>% dplyr::filter(!is.na(track.dz.album.id)) %>%
    dplyr::distinct(track.dz.album.id, .keep_all = TRUE) %>%
    dplyr::select(dplyr::contains('track.dz.album'))

  res <- dplyr::left_join(deezer_tracks_prejoin, deezer_artists_prejoin) %>%
    dplyr::left_join(deezer_albums_prejoin)
  res
}

