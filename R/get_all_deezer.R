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
#'@param threshold
#'Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
#'The values correspond to the string similarity (1 - Jaro-Winkler distance) between the track title on \emph{Spotify} and the found track title on \emph{Deezer}.
#'
#' @return Data Frame with added information from the \emph{Deezer} API using the  \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
get_all_deezer <- function(input, track_threshold = 0.8, album_threshold = 0.8, artist_threshold = 0.8){
  are_needed_columns_present(input, c('track.s.id', 'track.s.title', 'track.s.firstartist.name', 'album.s.title'))
  input <- rename_existing_variables(input, c(deezerTrackVars))

  input_distinct <- dplyr::distinct(input, track.s.id, track.s.firstartist.name, album.s.title, .keep_all = T) %>% dplyr::filter(! is.na(track.s.id))
  deezer_tracks <- purrr::pmap_df(list(input_distinct$track.s.title,
                                       input_distinct$track.s.firstartist.name,
                                       input_distinct$track.s.id,
                                       input_distinct$album.s.title),
                                  get_single_track_deezer, .progress = 'Retrieving tracks from Deezer...')


  deezer_artists <- lookup_firstartists_deezer(deezer_tracks)
  deezer_albums <- lookup_trackalbums_deezer(deezer_tracks)

  message('Done.')
  result <- suppressMessages(dplyr::left_join(deezer_tracks, deezer_artists) %>%
                     dplyr::left_join(deezer_albums))
  result <- filter_quality_deezer_all(result, track_threshold, album_threshold, artist_threshold)
  result <- suppressMessages(dplyr::left_join(input, result))
  print_linkage_for_id(result, 'track.dz.id')
  result
}

lookup_firstartists_deezer <- function(deezer_tracks){
  artist_urls <- purrr::map_chr(deezer_tracks$track.dz.firstartist.id, create_dz_artist_lookup_url)
  artist_urls[artist_urls == 'https://api.deezer.com/artist/NA'] <- NA
  purrr::map2_df(artist_urls, deezer_tracks$track.s.id, lookup_single_firstartist_deezer,
                 .progress = 'Looking up corresponding albums...')
}

lookup_single_firstartist_deezer <- function(url, track.s.id){
  if(is.na(url)){artist <- make_na_frame_deezer_artists(track.s.id)}
  else{
    artist_lookup <- get_api_with_connection_management(url)
    artist <- parse_dz_artist_lookup(artist_lookup, track.s.id)
  }
  dplyr::select(artist, track.s.id = artist.s.id,
                track.dz.firstartist.id = artist.dz.id,
                track.dz.firstartist.name = artist.dz.name,
                track.dz.firstartist.follower = artist.dz.follower,
                track.dz.firstartist.nalbums = artist.dz.nalbums,
                track.dz.firstartist.toptracks = artist.dz.toptracks)
}


lookup_trackalbums_deezer <- function(deezer_tracks){
  album_urls <- purrr::map_chr(deezer_tracks$track.dz.album.id, create_dz_album_lookup_url)
  album_urls[album_urls == 'https://api.deezer.com/album/NA'] <- NA
  purrr::map2_df(album_urls, deezer_tracks$track.s.id, lookup_single_trackalbum_deezer,
                 .progress = 'Looking up corresponding albums...')
}

lookup_single_trackalbum_deezer <- function(url, track.s.id){
  if(is.na(url)){album <- make_na_frame_deezer_albums(track.s.id)}
  else{
    album_lookup <- get_api_with_connection_management(url)
    album <- parse_dz_album_lookup(album_lookup, track.s.id)
  }
  dplyr::select(album, track.s.id = album.s.id,
                track.dz.album.title = album.dz.title,
                track.dz.album.upc = album.dz.upc,
                track.dz.album.totaltracks = album.dz.totaltracks,
                track.dz.album.duration = album.dz.duration,
                track.dz.album.follower = album.dz.follower,
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
