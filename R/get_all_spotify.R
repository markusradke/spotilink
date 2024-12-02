#'Get \emph{Spotify} API Information
#'
#'Retrieve a data frame containing information from the \emph{Spotify} API.
#'The result contains information on the tracks as well as corresponding albums and artists.
#'The \pkg{spotilink} naming convention is used.
#'
#' @param input
#'Data Frame containing a column \code{track.s.id} with \emph{Spotify} track ids.
#' @param pass
#'Character Vector containing two entries: \emph{Client ID} and \emph{Client secret}. See \url{https://developer.spotify.com/documentation/web-api/concepts/authorization} for details.
#'
#' @return Data Frame with added information from the \emph{Spotify} Web API using the \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
get_all_spotify <- function(input, pass) {
  are_needed_columns_present(input, c('track.s.id'))
  renameVars <- spotifyAllVars[! spotifyAllVars %in% c('track.s.id')]
  input <- rename_existing_variables(input, renameVars)
  input_ready <- input %>% dplyr::filter(!is.na(track.s.id))

  if(nrow(input_ready) == 0){
    res <- (handle_empty_input(input_ready, 'get_all_spotify', 'make_na_frame_spotify_all'))
  }
  else{
    connect_spotify(pass)
    res <- pull_tracks_spotify(input_ready)
    res <- dplyr::select(res, -'album.s.title')
    suppressMessages(connect_spotify(pass))
    res <- pull_albums_spotify(res)
    res <- expand_artists(res)
    res <- pull_artists_spotify(res)
  }

  na_ids <- input %>% dplyr::filter(is.na(track.s.id))
  if(nrow(na_ids) > 0){
    res <- na_ids %>% dplyr::select(-track.s.id) %>%
      cbind(make_na_frame_spotify_all(NA)) %>%
      rbind(res, .)
  }
  saveRDS(res, 'spotify.rds')
  res
}

expand_artists <- function(trackframe){
  trackframe <- dplyr::mutate(trackframe, 'track.s.artistlist' = .data[['track.s.artists']])
  idflag <- FALSE
  if('id' %in% colnames(trackframe)) {
    trackframe <- dplyr::rename(trackframe, 'id_temp' = 'id')
    idflag <- TRUE
  }
  trackframe <- trackframe %>%
    tidyr::unnest(cols = 'track.s.artistlist') %>%
    dplyr::rename('artist.s.name' = 'name',
           'artist.s.id' = 'id',) %>%
    dplyr::select(-'href',-'type',-'uri',-'external_urls.spotify')
  if(idflag) {trackframe <- dplyr::rename(trackframe, 'id' = 'id_temp')}
  return(trackframe)
}
