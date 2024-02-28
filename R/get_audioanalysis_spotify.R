#'Get \emph{Spotify} API Audio Analysis for Tracks
#'
#'Retrieve a data frame containing information from the \emph{Spotify} API.
#'The result contains tracks' audio analysis using the  \pkg{spotilink} naming convention.
#'
#' @param input
#'Data Frame containing a column \code{track.s.id} with \emph{Spotify} track ids.
#' @param pass
#'Character Vector containing two entries: \emph{Client ID} and \emph{Client secret}. See \url{https://developer.spotify.com/documentation/web-api/concepts/authorization} for details.
#'
#' @return Data Frame with added information from the \emph{Spotify} Web API using the \pkg{spotilink} naming convention. For retrieving further metadata on tracks, please use the \code{\link{get_tracks_spotify}} function.
#' @export
#'
#'@examples
get_audioanalysis_spotify <- function(input, pass) {
  if(! all(c('track.s.id') %in% colnames(input))) {
    stop('Please provide a data frame containing the following column: track.s.id. See the function reference for further information.')
  }

  renameVars <- spotifyAudioanalysisVars[! spotifyAudioanalysisVars %in% c('track.s.id')]
  res <- rename_existing_variables(input, renameVars)

  connect_spotify(pass)
  res <- retrieve_audioanalysis_spotify(res)
}

retrieve_audioanalysis_spotify <- function(input) {
  cat('Retrieving data with get_track_audio_analysis from identifier track.s.id ... \n')
  input %>%
    dplyr::distinct(.data[['track.s.id']]) %>%
    dplyr::pull('track.s.id') %>%
    purrr::map_df(retrieve_single_audioanalysis)
}

retrieve_single_audioanalysis <- function(track.s.id) {
  spotifyr::get_track_audio_analysis(track.s.id) %>%
  clean_analysis() %>%
  cbind(track.s.id, .)
}

clean_analysis <- function(analysisRaw) {
  dplyr::tibble(track.s.tempoconfidence = analysisRaw$track$tempo_confidence,
                track.s.timesignatureconfidence = analysisRaw$track$time_signature_confidence,
                track.s.keyconfidence = analysisRaw$track$key_confidence,
                track.s.modeconfidence = analysisRaw$track$mode_confidence,
                track.s.bars = list(analysisRaw$bars),
                track.s.beats = list(analysisRaw$beats),
                track.s.sections = list(analysisRaw$sections),
                track.s.segments = list(analysisRaw$segments),
                track.s.tatums = list(analysisRaw$tatums))
}
