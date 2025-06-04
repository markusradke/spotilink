#'Get \emph{Spotify} API Information on Tracks
#'
#'Retrieve a data frame containing information from the \emph{Spotify} API.
#'The result contains information on the tracks using the  \pkg{spotilink} naming convention.
#'
#' @param input
#'Data Frame containing a column \code{track.s.id} with \emph{Spotify} track ids.
#' @param pass
#'Character Vector containing two entries: \emph{Client ID} and \emph{Client secret}. See \url{https://developer.spotify.com/documentation/web-api/concepts/authorization} for details.
#'
#' @return Data Frame with added information from the \emph{Spotify} Web API using the \pkg{spotilink} naming convention. Tracks' audio analysis is not returned. Please use the \link{get_audioanalysis_spotify} function to achieve that.
#' @export
#'
#'@examples
get_tracks_spotify <- function(input, pass, pwd = 'none') {
  are_needed_columns_present(input, c('track.s.id'))
  renameVars <- spotifyTrackVars[! spotifyTrackVars %in% c('track.s.id')]
  input <- rename_existing_variables(input, renameVars)
  input_ready <- input %>% dplyr::filter(!is.na(track.s.id))

  if(nrow(input_ready) == 0){
    res <- handle_empty_input(input_ready, 'get_tracks_spotify', 'make_na_frame_spotify_tracks')
  }
  else{
    connect_spotify(pass)
    res <- pull_tracks_spotify(input_ready, pwd)

  }
  na_ids <- input %>% dplyr::filter(is.na(track.s.id))
  if(nrow(na_ids) > 0){
    res <- na_ids %>% dplyr::select(-track.s.id) %>%
      cbind(make_na_frame_spotify_tracks(NA)) %>%
      dplyr::bind_rows(res, .)
  }
  saveRDS(res, 'spotify_tracks.rds')
  if(file.exists('spotify_tracks_without_audiofeatures.rds')){
    file.remove('spotify_tracks_without_audiofeatures.rds')
  }
  res
}

pull_tracks_spotify <- function(input, pwd) {
  if(pwd == 'spotivey'){
    suppressMessages(connect_spotify(spotivey_pass))
    if(! file.exists('spotify_tracks_audiofeatures.rds')){
      res <- get_from_API(input, 'track.s.id', spotifyr::get_track_audio_features, clean_features, batchsize = 50)
      saveRDS(res, 'spotify_tracks_audiofeatures.rds')
    }
    else {res <- readRDS('spotify_tracks_audiofeatures.rds')}
  }
  else{
    res <- input
  }
  res <- get_from_API(res, 'track.s.id', spotifyr::get_tracks, clean_tracks, batchsize = 50)
  if(pwd != 'spotivey'){res <- res %>% dplyr::select(-track.s.previewurl)}
  tibble::as_tibble(res)
}

clean_tracks <- function(tracksRaw) {
  tracksRaw %>%
    dplyr::mutate(name = .data[['name']]) %>%
    dplyr::mutate(album.name = .data[['album.name']]) %>%
    dplyr::mutate(track.s.duration = .data[['duration_ms']] * 0.001) %>%
    tidyr::hoist('artists', track.s.firstartist.id = list('id', 1L), .remove = FALSE) %>%
    tidyr::hoist('artists', track.s.firstartist.name = list('name', 1L), .remove = FALSE) %>%
    dplyr::rename('track.s.artists' = 'artists') %>%
    dplyr::select(
      'track.s.id' = 'id',
      'track.s.title' = 'name',
      'track.s.firstartist.id',
      'track.s.firstartist.name',
      'track.s.artists',
      'track.s.explicitlyrics' = 'explicit',
      'track.s.popularity' = 'popularity',
      'track.s.isrc' = 'external_ids.isrc',
      'track.s.durationms' = 'duration_ms',
      'track.s.duration',
      'track.s.popularity' = 'popularity',
      'track.s.albumposition' = 'track_number',
      'track.s.previewurl' = 'preview_url',
      'album.s.id' = 'album.id',
      'album.s.title' = 'album.name'
    )
}

clean_features <- function(featuresRaw){
  keyLookup <- c('C', 'C#', 'D', 'Eb', 'E', 'F', 'F#', 'G', 'G#', 'A', 'Bb', 'B', 'no')
  featuresRaw %>%
    dplyr::mutate(time_signature = .data[['time_signature']] %>% as.character() %>% stringr::str_c('/4')) %>%
    dplyr::mutate(key = keyLookup[.data[['key']] + 1]) %>% # +1 due to 1-indexing in R vs. 0-indexing of keys in API
    dplyr::mutate(mode = ifelse(.data[['mode']] == 1, 'major', 'minor')) %>%
    dplyr::mutate(instrumentalness = .data[['instrumentalness']] %>% as.double()) %>%
    dplyr::select('track.s.id' = 'id',
                  'track.s.danceability' = 'danceability',
                  'track.s.energy' = 'energy',
                  'track.s.key' = 'key',
                  'track.s.loudness' = 'loudness',
                  'track.s.mode' = 'mode',
                  'track.s.speechiness' = 'speechiness',
                  'track.s.acousticness' = 'acousticness',
                  'track.s.instrumentalness' = 'instrumentalness',
                  'track.s.liveness' = 'liveness',
                  'track.s.valence' = 'valence',
                  'track.s.tempo' = 'tempo',
                  'track.s.timesignature' = 'time_signature')
}
