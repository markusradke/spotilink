#'Get \emph{Spotify} API Audio Analysis for Tracks
#'#' DEPRECATED - API does not allow for recommendation retrieval any more since November 2024
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
  are_needed_columns_present(input, c('track.s.id'))
  renameVars <- spotifyAudioanalysisVars[! spotifyAudioanalysisVars %in% c('track.s.id')]
  res <- rename_existing_variables(input, renameVars)


  connect_spotify(s_pass) # use SPOTIVEY pass
  res <- retrieve_audioanalysis_spotify(res)
}

retrieve_audioanalysis_spotify <- function(input) {
  cat('Retrieving data with get_track_audio_analysis from identifier track.s.id ... \n')
  unique_ids <- input %>%
    dplyr::distinct(.data[['track.s.id']]) %>%
    dplyr::filter(! is.na(track.s.id)) %>%
    dplyr::pull('track.s.id')

  checkpoint_name <- 'spotify_audioanalysis'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  saved_data <- checkpoint$saved_data
  if(last_index > 0) {unique_ids <- tail(unique_ids, -last_index)}
  purrr::map_df(unique_ids,
                retrieve_single_audioanalysis %>% save_checkpoint_and_count(checkpoint_name, last_index, saved_data),
                .progress = TRUE)
  res <- suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
  res <- suppressMessages(dplyr::right_join(res, input))
  save_file_and_remove_checkpoints(res, checkpoint_name)
  res
}

retrieve_single_audioanalysis <- function(track.s.id) {
  res <- spotify_api_connection_management(id = track.s.id, spotify_function = spotifyr::get_track_audio_analysis)
  Sys.sleep(2)
  res %>%
    clean_analysis() %>%
    cbind(track.s.id, .)
}

clean_analysis <- function(analysisRaw) {
  keyLookup <- c('C', 'C#', 'D', 'Eb', 'E', 'F', 'F#', 'G', 'G#', 'A', 'Bb', 'B', 'no')
  sections <- analysisRaw$sections %>%
    dplyr::mutate(time_signature = .data[['time_signature']] %>% as.character() %>% stringr::str_c('/4')) %>%
    dplyr::mutate(key = keyLookup[.data[['key']] + 1]) %>%
    dplyr::mutate(mode = ifelse(.data[['mode']] == 1, 'major', 'minor'))

  dplyr::tibble(track.s.tempoconfidence = analysisRaw$track$tempo_confidence,
                track.s.timesignatureconfidence = analysisRaw$track$time_signature_confidence,
                track.s.keyconfidence = analysisRaw$track$key_confidence,
                track.s.modeconfidence = analysisRaw$track$mode_confidence,
                track.s.bars = list(analysisRaw$bars),
                track.s.beats = list(analysisRaw$beats),
                track.s.sections = list(sections),
                track.s.segments = list(analysisRaw$segments),
                track.s.tatums = list(analysisRaw$tatums),
                track.s.synchstring = analysisRaw$track$synchstring,
                track.s.rhythmstring = analysisRaw$track$rhythmstring,
                track.s.echoprintstring = analysisRaw$track$echoprintstring,
                track.s.codestring = analysisRaw$track$codestring)
}


spotify_api_connection_management <- function(id, spotify_function){
  repeat {
    tryCatch({
      res <- spotify_function(id)
      return(res)
    }, error = function(e) {
      if (grepl("429", e$message)) {
        message("Received 429 error. Waiting for 45 seconds before retrying...")
      } else {
        message("An error occurred: ", e$message)
      }
      Sys.sleep(45)
    })
  }
}
