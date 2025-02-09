#'Get \emph{Deezer} Artist Information
#'
#'Retrieve a data frame containing information from the \emph{Deezer} API. This function searches for artists based on the artist name from Spotify.
#'The result contains information on artists.
#'The \pkg{spotilink} naming convention is used.
#'
#' @param input
#'Data Frame containing the following columns:
#'\itemize{
#'  \item\code{artist.s.id} \cr
#'  with \emph{Spotify} artist ids,
#'  \item \code{artist.s.name} \cr
#'  with \code{Spotify} name of first artist,
#'}
#'It is advisable to first run \code{\link{get_tracks_spotify}} before running this command,
#'in order to have all the necessary information.
#'
#'@param threshold
#'Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
#'The values correspond to the string similarity (1 - Jaro-Winkler distance) between the track title on \emph{Spotify} and the found track title on \emph{Deezer}.
#'
#'
#' @return Data Frame with added information from the \emph{Deezer} API using the  \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
get_artists_deezer <- function(input, threshold = 0.8){
  are_needed_columns_present(input, c('artist.s.id', 'artist.s.name'))
  input <- rename_existing_variables(input, deezerArtistVars)
  input_distinct <- dplyr::distinct(input, artist.s.id, artist.s.name, .keep_all = T) %>% dplyr::filter(! is.na(artist.s.id))

  checkpoint_name <- 'deezer_artists'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  saved_data <- checkpoint$saved_data
  if(last_index > 0) {input_distinct <- tail(input_distinct, -last_index)}
  purrr::pmap_df(list(input_distinct$artist.s.name,
                      input_distinct$artist.s.id),
                      get_single_artist_deezer %>% save_checkpoint_and_count(checkpoint_name, last_index, saved_data),
                      .progress = 'Retrieving artists from Deezer...')

  deezer_artists <- suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
  deezer_artists <- filter_quality_deezer_artists(deezer_artists, threshold)
  message('Done.')
  result <- suppressMessages(dplyr::left_join(input, deezer_artists))
  print_linkage_for_id('artist.dz.id', result)
  save_file_and_remove_checkpoints(result, checkpoint_name)
  result
}

get_single_artist_deezer <- function(artist.s.name, artist.s.id){
  .create_search_url <- function(artist.s.name){
    paste0('https://api.deezer.com/search?q=artist:"', artist.s.name %>% simplify_name(),'"') %>%
           utils::URLencode()
  }

  .get_parsed_topresult <- function(result){
    topresults <- data.frame(artist.dz.id = sapply(result$data, function(hit) hit$artist$id %>% as.character()),
                             artist.dz.name = sapply(result$data, function(hit) hit$artist$name))
    if(nrow(topresults) == 0){return(NULL)}
    topresults %>%
      dplyr::mutate(artist.dz.quality = stringdist::stringsim(artist.s.name %>% simplify_name(), artist.dz.name %>% simplify_name())) %>%
      dplyr::arrange(-artist.dz.quality, artist.dz.id) %>%
      dplyr::mutate(artist.dz.id = artist.dz.id %>% as.character()) %>%
      dplyr::first()
  }

  url <- .create_search_url(artist.s.name)
  result <- get_api_with_connection_management(url)
  topresult <- .get_parsed_topresult(result)
  if(is.null(topresult)){return(make_na_frame_deezer_artists(artist.s.id))}

  url <- create_dz_artist_lookup_url(topresult$artist.dz.id)
  artist_lookup <- get_api_with_connection_management(url)
  res <- parse_dz_artist_lookup(artist_lookup, artist.s.id)
  return(suppressMessages(dplyr::inner_join(res, topresult) %>% dplyr::mutate(artist.s.id = artist.s.id)))
}


create_dz_artist_lookup_url <- function(artist.dz.id){
  paste0('https://api.deezer.com/artist/', artist.dz.id)
}

parse_dz_artist_lookup <- function(lookup, artist.s.id){
  if(length(lookup$tracklist) == 0){
    lookup$tracklist <- NA
  }
  parsed <- data.frame(artist.s.id,
                       artist.dz.id = lookup$id %>% as.character(),
                       artist.dz.name = lookup$name,
                       artist.dz.nalbums = lookup$nb_album,
                       artist.dz.followers = lookup$nb_fan,
                       artist.dz.toptracks = lookup$tracklist)
  toptracks <- list(get_api_with_connection_management(parsed$artist.dz.toptracks)$data)
  parsed$artist.dz.toptracks <- toptracks
  parsed
}

