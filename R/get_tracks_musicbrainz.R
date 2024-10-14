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
#'  \item \code{track.s.firstartist.name} \cr
#'  with \code{Spotify} name of first artist,
#'  \item \code{track.s.isrc} \cr
#'  with track ISRCs from \emph{Spotify}
#'}
#'It is advisable to first run \code{\link{get_tracks_spotify}} before running this command,
#'in order to have all the necessary information.
#'
#'@param track_threshold,firstartist_threshold
#'Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
#'The values correspond to the string similarity (1 - Jaro-Winkler distance) between the track title on \emph{Spotify} and the found track title on \emph{Musicbrainz}.
#'
#' @return Data Frame with added information from the \emph{MusicBrainz} API using the  \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
#'data <- data.frame(track.s.id = c('4ZXLWTmQFzM02hZwMiZfgS'),
#'                   track.s.title = c('Der Geist hilft unser Schwachheit auf, BWV 226'),
#'                   track.s.isrc = c('GBHNG1200003'),
#'                   track.s.firstartist.name = c('Johann Sebastian Bach'))
#'
#'get_tracks_musicbrainz(data)
get_tracks_musicbrainz <- function(input, track_threshold = 0.8, firstartist_threshold = 0.8) {
  are_needed_columns_present(input, c('track.s.id', 'track.s.title', 'track.s.isrc', 'track.s.firstartist.name'))
  res <- rename_existing_variables(input, musicbrainzTrackVars)

  pull_tracks_musicbrainz(res, track_threshold, firstartist_threshold)
}

pull_tracks_musicbrainz <- function(input, track_threshold, firstartist_threshold) {
  distinctinput <- input %>%
    dplyr::filter(! is.na(track.s.id)) %>%
    dplyr::distinct(.data[['track.s.id']], .keep_all = TRUE)
  result <- search_tracks_mbids(distinctinput)
  result <- lookup_tracks_mb(result)

  result <- filter_quality_musicbrainz_acousticbrainz_tracks(result, track_threshold, firstartist_threshold)
  result <- dplyr::left_join(input, result, by = c('track.s.id'))
  saveRDS(result, 'mb_tracks.rds')
  mbtracks_remove_checkpoints()
  result
}

search_tracks_mbids <- function(distinctinput){
  checkpoint_name <- 'mb_tracks_search'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  if(! last_index == nrow(distinctinput)){
    saved_data <- checkpoint$saved_data
    if(last_index > 0) {distinctinput <- tail(distinctinput, -last_index)}
    purrr::pmap_df(list(distinctinput$track.s.id,
                                  distinctinput$track.s.title,
                                  distinctinput$track.s.firstartist.name,
                                  distinctinput$track.s.isrc),
                             search_single_track_mbid %>% save_checkpoint_and_count(checkpoint_name,
                                                                                    last_index,
                                                                                    saved_data,
                                                                                    savingstep = 20,
                                                                                    ndatapoints = nrow(distinctinput)),
                             .progress = 'Searching tracks on Musicbrainz...')
  }
  else{message('Track search already done.')}
  result <- suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
  n_found_by_isrc <- result %>% dplyr::filter(track.mb.foundbyisrc) %>% nrow()
  message(paste0(round(n_found_by_isrc / nrow(distinctinput) * 100, 2), '% distinct track were found using the ISRC from the Spotify track information.'))
  result
}

search_single_track_mbid <- function(track.s.id, track.s.title, track.s.firstartist.name, track.s.isrc) {
  result <- find_tracks_with_ISRC(track.s.isrc)
  if  (nrow(result) == 0) {
    result <- find_tracks_without_ISRC(track.s.firstartist.name, track.s.title)
  }
  result %>% dplyr::mutate(track.s.id = track.s.id) %>%
    dplyr::select('track.s.id',
                  'track.mb.id' = 'mbid',
                  'track.mb.title' = 'title',
                  'track.mb.quality',
                  'track.mb.foundbyisrc',
                  'track.mb.artistlist' = 'artists',
                  'track.mb.firstartist.id',
                  'track.mb.firstartist.name',
                  'track.mb.firstartist.quality',
                  'track.mb.releases' = 'releases')
}

find_tracks_with_ISRC <- function(track.s.isrc) {
  result <- suppressMessages(musicbrainz::search_recordings(paste0('isrc:', track.s.isrc)))
  if (nrow(result) != 0){
    result <- result[1,] %>%
      tidyr::hoist('artists', track.mb.firstartist.id = list('artist_mbid', 1L), .remove = FALSE) %>%
      tidyr::hoist('artists', track.mb.firstartist.name = list('name', 1L), .remove = FALSE) %>%
      dplyr::mutate(track.mb.quality = 1, track.mb.firstartist.quality = 1, track.mb.foundbyisrc = TRUE)
  }
  result
}

find_tracks_without_ISRC <- function(track.s.firstartist.name, track.s.title) {
  search <- suppressMessages(musicbrainz::search_recordings(paste0('artist:', track.s.firstartist.name,' and recording:', track.s.title)))
  search %>%  tidyr::hoist('artists', track.mb.firstartist.id = list('artist_mbid', 1L), .remove = FALSE) %>%
    tidyr::hoist('artists', track.mb.firstartist.name = list('name', 1L), .remove = FALSE) %>%
    dplyr::mutate(track.mb.quality = stringdist::stringsim(track.s.title %>% simplify_name(),
                                                           title%>% simplify_name(),
                                                           'jw'),
                  track.mb.firstartist.quality = stringdist::stringsim(track.s.firstartist.name %>% simplify_name(),
                                                                       track.mb.firstartist.name %>% simplify_name(),
                                                                       'jw'),
                  track.mb.foundbyisrc = FALSE) %>%
    dplyr::arrange(-track.mb.quality, -track.mb.firstartist.quality, -score) %>%
    dplyr::first()
}

lookup_tracks_mb <- function(track_mbids){
  checkpoint_name <- 'mb_tracks_lookup'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  if(! last_index == nrow(track_mbids)){
    saved_data <- checkpoint$saved_data
    if(last_index > 0) {track_mbids <- tail(track_mbids, -last_index)}
    purrr::map_df(track_mbids$track.mb.id,
                  lookup_single_track_mb %>% save_checkpoint_and_count(checkpoint_name,
                                                                       last_index,
                                                                       saved_data,
                                                                       savingstep = 20,
                                                                       ndatapoints = nrow(track_mbids)),
                 .progress = 'Looking up track genres on Musicbrainz...')
  }
  else{message('Track lookup already done.')}
  trackGenres <- suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
  cbind(track_mbids, trackGenres)
}

lookup_single_track_mb  <- function(mbID) {
  musicbrainz::lookup_recording_by_id(mbID, includes=c('tags')) %>%
    dplyr::mutate(score = .data[['score']] %>% as.character()) %>%
    dplyr::mutate(length = .data[['length']] %>% as.integer()) %>%
    get_highest_ranking_genre() %>%
    dplyr::rename('track.mb.genres' = 'genres',
                  'track.mb.topgenre' = 'topgenre') %>%
    dplyr::mutate(track.mb.topgenre = .data[['track.mb.topgenre']] %>% as.character())
}

mbtracks_remove_checkpoints <- function(){
  pattern <- paste0('^mb_tracks_(search|lookup)_(\\d+)\\.rds$')
  files <- list.files()
  matching_files <- grep(pattern, files, value = TRUE)
  for(file in matching_files){file.remove(file)}
}

