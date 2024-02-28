#'Get \emph{MusicBrainz} Track Information
#'
#'Retrieve a data frame containing information from the \emph{MusicBrainz} API.
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
#'  \item \code{track.s.isrc} \cr
#'  with track ISRCs from \emph{Spotify}
#'}
#'It is advisable to first run \code{\link{get_tracks_spotify}} before running this command,
#'in order to have all the necessary information.
#'
#'@param threshold
#'Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
#'The values correspond to the string similarity (1 - Jaro-Winkler distance) between the track title on \emph{Spotify} and the found track title on \emph{Musicbrainz}.
#'
#' @return Data Frame with added information from the \emph{MusicBrainz} API using the  \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
get_tracks_musicbrainz <- function(input, threshold = 0.8) {
  if(! all(c('track.s.id', 'track.s.title', 'track.s.isrc') %in% colnames(input))) {
    stop('Please provide a data frame containing the following columns: track.s.id, track.s.title, track.s.irsc. See the function reference for further information.')
  }

  renameVars <- musicbrainzTrackVars[! musicbrainzTrackVars %in% c('track.s.id', 'track.s.title', 'track.s.isrc')]
  res <- rename_existing_variables(input, renameVars)

  pull_tracks_musicbrainz(res, threshold)
}

pull_tracks_musicbrainz <- function(input, threshold) {
  res <- input %>%
    dplyr::distinct(.data[['track.s.id']], .keep_all = TRUE) %>%
    retrieve_tracks()
  res %>%
    retrieve_track_genre() %>%
    dplyr::left_join(input, .data, by = c('track.s.id')) %>%
    filter_low_quality('track', threshold)
}

retrieve_tracks <- function(distinctinput){
  mbTracks <- c()
  isrcCounter <<- 0L
  n <- nrow(distinctinput)
  cat('---------------------------------------------------\n')
  cat('Looking for tracks in Musicbrainz...\n')
  for (i in 1:nrow(distinctinput)){
    cat('---------------------------------------------------\n')
    cat('track', i, 'of', n, '\n')
    result <- find_tracks_with_ISRC(distinctinput[i,])
    if  (nrow(result) == 0) {
      result <- find_tracks_without_ISRC(distinctinput[i,])
    }
    mbTracks <- rbind(mbTracks, result)
  }
  cat('---------------------------------------------------\n')
  cat(paste0(round(isrcCounter / nrow(distinctinput),4) * 100, '% of tracks were found using the ISRC.\n'))
  rm(isrcCounter, pos = .GlobalEnv)
  mbTracks <- mbTracks %>%
    tidyr::hoist('artists', track.mb.firstartist.id = list('artist_mbid', 1L), .remove = FALSE) %>%
    tidyr::hoist('artists', track.mb.firstartist.name = list('name', 1L), .remove = FALSE) %>%
    dplyr::select('track.mb.id' = 'mbid',
                  'track.mb.title' = 'title',
                  'track.mb.quality',
                  'track.mb.artistlist' = 'artists',
                  'track.mb.firstartist.id',
                  'track.mb.firstartist.name',
                  'track.mb.releases' = 'releases')
  cbind(track.s.id = distinctinput$track.s.id, mbTracks)
}

find_tracks_with_ISRC <- function(observation) {
  result <- musicbrainz::search_recordings(paste0('isrc:', observation$track.s.isrc))
  if (nrow(result) != 0){
    cat('found via ISRC\n')
    isrcCounter <<- isrcCounter + 1
    result <- result[1,] %>%
      dplyr::mutate(track.mb.quality = 1)
  }
  result
}

find_tracks_without_ISRC <- function(observation) {
  cat('no ISRC, searching...\n')
  musicbrainz::search_recordings(paste0('artist:', observation$track.firstartist.name,' and recording:', observation$track.s.title)) %>%
    .[1,] %>%
    dplyr::mutate(track.mb.quality = calculate_and_print_quality(search = observation$track.s.title,
                                                                 found = .data[['title']]))
}

retrieve_track_genre <- function(tracks){
  cat('Looking up track genre...\n')
  trackGenres <- purrr::map_df(tracks$track.mb.id, lookup_musibrainz_track_tags_from_ID, .progress = TRUE) %>%
    get_highest_ranking_genre() %>%
    dplyr::rename('track.mb.genres' = 'genres',
                  'track.mb.topgenre' = 'topgenre') %>%
    dplyr::mutate(track.mb.topgenre = .data[['track.mb.topgenre']] %>% as.character())
  cbind(tracks, trackGenres)
}

lookup_musibrainz_track_tags_from_ID  <- function(mbID) {
  musicbrainz::lookup_recording_by_id(mbID, includes=c('tags')) %>%
    dplyr::mutate(score = .data[['score']] %>% as.character()) %>%
    dplyr::mutate(length = .data[['length']] %>% as.integer())
}
