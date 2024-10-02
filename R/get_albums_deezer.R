#'Get \emph{Deezer} Album Information
#'
#'Retrieve a data frame containing information from the \emph{Deezer} API. This function searches for albums based on the album title and artist name from Spotify.
#'The result contains information on albums.
#'The \pkg{spotilink} naming convention is used.
#'
#' @param input
#'Data Frame containing the following columns:
#'\itemize{
#'  \item\code{album.s.id} \cr
#'  with \emph{Spotify} album ids,
#'  \item \code{album.s.title} \cr
#'  with \emph{Spotify} album title,
#'  \item \code{track.s.firstartist.name} \cr
#'  with \code{Spotify} name of first artist,
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
get_albums_deezer <- function(input, album_threshold = 0.8, artist_threshold = 0.8){
  are_needed_columns_present(input, c('album.s.id', 'album.s.title', 'track.s.firstartist.name'))
  input <- rename_existing_variables(input, deezerAlbumVars)
  input_distinct <- dplyr::distinct(input, album.s.id, track.s.firstartist.name, .keep_all = T)

  deezer_albums <- purrr::pmap_df(list(input_distinct$album.s.title,
                                       input_distinct$track.s.firstartist.name,
                                       input_distinct$album.s.id),
                                       get_single_album_deezer, .progress = 'Retrieving albums from Deezer...')

  deezer_albums <- filter_quality_deezer_albums(deezer_albums, album_threshold, artist_threshold)
  message('Done.')
  result <- suppressMessages(dplyr::left_join(input, deezer_albums))
  print_linkage_for_id(result, 'album.dz.id')
  result
}

get_single_album_deezer <- function(album.s.title, track.s.firstartist.name, album.s.id){
  .create_search_url <- function(album.s.title, track.s.firstartist.name){
    paste0('https://api.deezer.com/search?q=artist:"', track.s.firstartist.name %>% simplify_name(),
           '" album:"', album.s.title %>% simplify_name(), '"') %>% utils::URLencode()
  }

  .get_parsed_topresult <- function(result){
    topresults <- data.frame(album.dz.id = sapply(result$data, function(hit) hit$album$id),
                             album.dz.title = sapply(result$data, function(hit) hit$album$title),
                             album.dz.firstartist.id = sapply(result$data, function(hit) hit$artist$id %>% as.character()),
                             album.dz.firstartist.name = sapply(result$data, function(hit) hit$artist$name))
    if(nrow(topresults) == 0){return(NULL)}
    topresults %>%
      dplyr::mutate(album.dz.firstartist.quality = stringdist::stringsim(track.s.firstartist.name %>% simplify_name(), album.dz.firstartist.name %>% simplify_name()),
                    album.dz.quality = stringdist::stringsim(album.s.title %>% simplify_name(), album.dz.title %>% simplify_name())) %>%
      dplyr::arrange(-album.dz.firstartist.quality, -album.dz.quality, album.dz.id) %>%
      dplyr::mutate(album.dz.id = album.dz.id %>% as.character()) %>%
      dplyr::first()
  }

  url <- .create_search_url(album.s.title, track.s.firstartist.name)
  result <- get_api_with_connection_management(url)
  topresult <- .get_parsed_topresult(result)
  if(is.null(topresult)){return(make_empty_frame_deezer_albums(album.s.id))}

  url <- create_dz_album_lookup_url(topresult$album.dz.id)
  album_lookup <- get_api_with_connection_management(url)
  res <- parse_dz_album_lookup(album_lookup, album.s.id)
  return(suppressMessages(dplyr::inner_join(res, topresult) %>% dplyr::mutate(album.s.id = album.s.id)))
}


create_dz_album_lookup_url <- function(album.dz.id){
  paste0('https://api.deezer.com/album/', album.dz.id)
}

parse_dz_album_lookup <- function(lookup, album.s.id){
  parsed <- data.frame(album.s.id,
                       album.dz.id = lookup$id %>% as.character(),
                       album.dz.title = lookup$title,
                       album.dz.upc = lookup$upc,
                       album.dz.totaltracks = lookup$nb_tracks,
                       album.dz.duration = lookup$duration,
                       album.dz.follower = lookup$fans,
                       album.dz.releasedate = lookup$release_date %>% as.Date(),
                       album.dz.type = lookup$record_type,
                       album.dz.explicitlyrics = lookup$explicit_lyrics,
                       album.dz.explicitlyricsinfo = lookup$explicit_content_lyrics,
                       album.dz.explicitcoverinfo = lookup$explicit_content_cover,
                       album.dz.label = lookup$label) %>%
    dplyr::mutate(album.dz.explicitlyricsinfo = purrr::map_chr(.data$album.dz.explicitlyricsinfo, decode_explicit_info),
                  album.dz.explicitcoverinfo = purrr::map_chr(.data$album.dz.explicitcoverinfo, decode_explicit_info))

  parsed$album.dz.genres  <- lookup$genres
  parsed <- tidyr::hoist(parsed, album.dz.genres, album.dz.firstgenre.name = list(1, 'name'))
  parsed <- tidyr::hoist(parsed, album.dz.genres, album.dz.firstgenre.id = list(1, 'id'))
  parsed
}


make_empty_frame_deezer_albums <- function(album.s.id){
  data.frame(album.s.id = album.s.id,
             album.dz.quality = NA,
             album.dz.title = NA,
             album.dz.upc = NA,
             album.dz.totaltracks = NA,
             album.dz.duration = NA,
             album.dz.follower = NA,
             album.dz.releasedate = NA,
             album.dz.type = NA,
             album.dz.explicitlyrics = NA,
             album.dz.explicitlyricsinfo = NA,
             album.dz.explicitcoverinfo = NA,
             album.dz.label = NA,
             album.dz.genres = NA,
             album.dz.firstgenre.id = NA,
             album.dz.firstgenre.name = NA,
             album.dz.firstartist.id = NA,
             album.dz.firstartist.name = NA,
             album.dz.firstartist.quality = NA)
}
