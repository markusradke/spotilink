#'Get \emph{Deezer} Track Information
#'
#'Retrieve a data frame containing information from the \emph{Deezer} API.
#'The result contains information on tracks.
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
get_tracks_deezer <- function(input, track_threshold = 0.8, artist_threshold = 0.8){
  are_needed_columns_present(input, c('track.s.id', 'track.s.title', 'track.s.firstartist.name', 'album.s.title'))
  input <- rename_existing_variables(input, c(deezerTrackVars))

  input_distinct <- dplyr::distinct(input, track.s.id, track.s.firstartist.name, album.s.title, .keep_all = T) %>% dplyr::filter(! is.na(track.s.id))
  deezer_tracks <- purrr::pmap_df(list(input_distinct$track.s.title,
                                input_distinct$track.s.firstartist.name,
                                input_distinct$track.s.id,
                                input_distinct$album.s.title),
                                get_single_track_deezer, .progress = 'Retrieving tracks from Deezer...')

  message('Done.')
  deezer_tracks <- filter_quality_deezer_tracks(deezer_tracks, track_threshold, artist_threshold)
  result <- suppressMessages(dplyr::left_join(input, deezer_tracks))
  print_linkage_for_id(result, 'track.dz.id')
  result
}

get_single_track_deezer <- function(track.s.title, track.s.firstartist.name, track.s.id, album.s.title){
  .create_search_url <- function(track.s.title, track.s.firstartist.name, album.s.title = NA){
    paste0('https://api.deezer.com/search?q=artist:"', track.s.firstartist.name %>% simplify_name(),
          '" track:"',track.s.title %>% simplify_name(),
          '" album:"', album.s.title %>% simplify_name(), '"') %>% utils::URLencode()
  }

  .get_parsed_topresult <- function(result){
    topresults <- data.frame(track.dz.id = sapply(result$data, function(hit) hit$id),
                             track.dz.title = sapply(result$data, function(hit) hit$title),
                             track.dz.firstartist.id = sapply(result$data, function(hit) hit$artist$id %>% as.character()),
                             track.dz.firstartist.name = sapply(result$data, function(hit) hit$artist$name),
                             track.dz.album.id = sapply(result$data, function(hit) hit$album$id %>% as.character()),
                             track.dz.album.title = sapply(result$data, function(hit) hit$album$title))
    if(nrow(topresults) == 0){return(NULL)}
    topresults %>%
      dplyr::mutate(track.dz.quality = stringdist::stringsim(track.s.title %>% simplify_name(), track.dz.title %>% simplify_name()),
                    track.dz.firstartist.quality = stringdist::stringsim(track.s.firstartist.name %>% simplify_name(), track.dz.firstartist.name %>% simplify_name()),
                    track.dz.album.quality = stringdist::stringsim(album.s.title %>% simplify_name(), track.dz.album.title %>% simplify_name())) %>%
      dplyr::arrange(-track.dz.firstartist.quality, -track.dz.quality, -track.dz.album.quality, track.dz.id) %>%
      dplyr::mutate(track.dz.id = track.dz.id %>% as.character()) %>%
      dplyr::first()
  }

  url <- .create_search_url(track.s.title, track.s.firstartist.name, album.s.title)
  result <- get_api_with_connection_management(url)
  topresult <- .get_parsed_topresult(result)
  if(is.null(topresult)){return(make_na_frame_deezer_tracks(track.s.id))}
  url <- create_dz_track_lookup_url(topresult$track.dz.id)
  track_lookup <- get_api_with_connection_management(url)
  res <- parse_dz_track_lookup(track_lookup)
  suppressMessages(dplyr::inner_join(res, topresult) %>% dplyr::mutate(track.s.id = track.s.id))
}

create_dz_track_lookup_url <- function(track.dz.id){
  paste0('https://api.deezer.com/track/', track.dz.id)
}

parse_dz_track_lookup <- function(lookup){
  data.frame(track.dz.id = lookup$id %>% as.character(),
             track.dz.title = lookup$title,
             track.dz.isrc = lookup$isrc,
             track.dz.duration = lookup$duration,
             track.dz.rank = lookup$rank,
             track.dz.explicit = lookup$explicit_lyrics,
             track.dz.explicitinfo = lookup$explicit_content_lyrics,
             track.dz.tempo = lookup$bpm,
             track.dz.loudness = lookup$gain) %>%
    dplyr::mutate(track.dz.explicitinfo = purrr::map_chr(.data$track.dz.explicitinfo, decode_explicit_info))
}
