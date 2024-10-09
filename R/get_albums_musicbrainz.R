#'Get \emph{MusicBrainz} Album Information
#'
#'Retrieve a data frame containing information from the \emph{MusicBrainz} API.
#'The result contains information on albums.
#'The \pkg{spotilink} naming convention is used.
#'
#'@param input
#'Data Frame containing the following columns:
#'\itemize{
#'  \item \code{album.s.id} \cr
#'  with \emph{Spotify} album id,
#'  \item \code{album.s.title} \cr
#'  with \emph{Spotify} album title,
#'  \item \code{album.s.upc} \cr
#'  with album UPCs from \emph{Spotify}.
#'}
#'It is advisable to first run \code{\link{get_albums_spotify}} before running this command,
#'in order to have all the necessary information.
#'
#'@param album_threshold
#'Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
#'The values correspond to the string similarity (1 - Jaro-Winkler distance) between the album title on \emph{Spotify} and the found album title on \emph{Musicbrainz}.
#'
#' @return Data Frame with added information from the \emph{MusicBrainz} API using the  \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
#'data <- data.frame(album.s.id = c('2rlWvQ1GuTOSkyNpmcoaMC'),
#'                   album.s.title = c('Bach: Motets'),
#'                   album.s.firstartist.name = c('Johann Sebastian Bach'),
#'                   album.s.releaseyear = c(2012),
#'                   album.s.upc = c('843183071623'))
#'
#'get_albums_musicbrainz(data)
get_albums_musicbrainz <- function(input, album_threshold = 0.8,artist_threshold = 0.8) {
  are_needed_columns_present(input, c('album.s.id', 'album.s.title', 'album.s.upc', 'album.s.firstartist.name', 'album.s.releaseyear'))
  res <- rename_existing_variables(input, musicbrainzAlbumVars)

  pull_albums_musicbrainz(res, album_threshold, artist_threshold)
}

pull_albums_musicbrainz <- function(input, album_threshold, artist_threshold) {
  distinctinput <- input %>%
    dplyr::filter(! is.na(album.s.id)) %>%
    dplyr::distinct(.data[['album.s.id']], .keep_all = TRUE)
  result <- search_albums_mbids(distinctinput)
  result <- lookup_albums_mb(result)
  result <- filter_quality_musicbrainz_albums(result, album_threshold, artist_threshold)
  result <- suppressMessages(dplyr::left_join(input, result, by = c('album.s.id')))
  saveRDS(result, 'mb_albums.rds')
  mbalbums_remove_checkpoints()
  result
}

search_albums_mbids <- function(distinctinput){
  checkpoint_name <- 'mb_albums_search'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  if(! last_index == nrow(distinctinput)){
    saved_data <- checkpoint$saved_data
    if(last_index > 0) {distinctinput <- tail(distinctinput, -last_index)}
  result <- purrr::pmap_df(list(distinctinput$album.s.id,
                                distinctinput$album.s.title,
                                distinctinput$album.s.firstartist.name,
                                distinctinput$album.s.releaseyear,
                                distinctinput$album.s.upc),
                             search_single_album_mbid %>% save_checkpoint_and_count(checkpoint_name, last_index, saved_data),
                             .progress = 'Searching for albums on Musicbrainz...')
  }
  else{message('Album search already done.')}
  result <- suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
  n_found_by_isrc <- result %>% dplyr::filter(album.mb.foundbyupc) %>% nrow()
  message(paste0(round(n_found_by_isrc / nrow(distinctinput) * 100, 2), '% distinct albums were found using the UPC from the Spotify album information.'))
  result
}

search_single_album_mbid <- function(album.s.id, album.s.title, album.s.firstartist.name, album.s.releaseyear, album.s.upc){
  result <- find_album_with_UPC(album.s.upc)
  if  (nrow(result) == 0) {
    result <- find_album_without_UPC(album.s.title, album.s.firstartist.name, album.s.releaseyear)
  }
  result <- result %>% dplyr::select('album.mb.id' = 'mbid',
                                     'album.mb.title' = 'title',
                                     'album.mb.language' = 'language',
                                     'album.mb.firstartist.id',
                                     'album.mb.firstartist.name',
                                     'album.mb.firstartist.quality',
                                     'album.mb.quality',
                                     'album.mb.foundbyupc') %>%
    dplyr::mutate(album.s.id = album.s.id)
  result
}

find_album_with_UPC <- function(album.s.upc) {
  result <- suppressMessages(musicbrainz::search_releases(paste0('barcode:', album.s.upc)))
  if (nrow(result) != 0){
    result <- result[1,] %>%
      tidyr::hoist('artists', album.mb.firstartist.id = list('artist_mbid', 1L), .remove = FALSE) %>%
      tidyr::hoist('artists', album.mb.firstartist.name = list('name', 1L), .remove = FALSE) %>%
      dplyr::mutate(album.mb.quality = 1, album.mb.firstartist.quality = 1, album.mb.foundbyupc = TRUE)
  }
  result
}

find_album_without_UPC <- function(album.s.title, album.s.firstartist.name, album.s.releaseyear) {
  result <- suppressMessages(musicbrainz::search_releases(paste0('artist:', album.s.firstartist.name,' and release:', album.s.title)))
  result %>%
    tidyr::hoist('artists', album.mb.firstartist.id = list('artist_mbid', 1L), .remove = FALSE) %>%
    tidyr::hoist('artists', album.mb.firstartist.name = list('name', 1L), .remove = FALSE) %>%
    dplyr::mutate(album.mb.quality = stringdist::stringsim(album.s.title %>% simplify_name(),
                                                           title%>% simplify_name(),
                                                           'jw'),
                  album.mb.firstartist.quality = stringdist::stringsim(album.s.firstartist.name %>% simplify_name(),
                                                                       album.mb.firstartist.name %>% simplify_name(),
                                                                       'jw'),
                  album.mb.releaseyear = stringr::str_sub(date, 1, 4) %>% as.integer(),
                  releasediff = abs(album.s.releaseyear - album.mb.releaseyear),
                  album.mb.foundbyupc = FALSE) %>%
    dplyr::arrange(-album.mb.firstartist.quality, -album.mb.quality, releasediff, -score) %>%
    dplyr::first() %>%
    dplyr::select(-releasediff, -album.mb.releaseyear)
}

lookup_albums_mb <- function(album_mbids){
  checkpoint_name <- 'mb_albums_lookup'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  if(! last_index == nrow(album_mbids)){
    saved_data <- checkpoint$saved_data
    if(last_index > 0) {album_mbids <- tail(album_mbids, -last_index)}
    purrr::map_df(album_mbids$album.mb.id,
                  lookup_single_album_mb %>% save_checkpoint_and_count(checkpoint_name, last_index, saved_data),
                  .progress = 'Looking up album genres on Musicbrainz...')
  }
  else{message('Album lookup already done.')}
  albumGenres <- suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
  cbind(album_mbids, albumGenres)
}


lookup_single_album_mb  <- function(mbID) {
  res <- musicbrainz::lookup_release_by_id(mbID, includes=c('tags'))
  res <- suppressWarnings(dplyr::mutate(res, score = .data[['score']] %>% as.character()))
  res %>%
    get_highest_ranking_genre() %>%
    dplyr::rename('album.mb.genres' = 'genres',
                  'album.mb.topgenre' = 'topgenre') %>%
    dplyr::mutate(album.mb.topgenre = .data[['album.mb.topgenre']] %>% as.character())
}

mbalbums_remove_checkpoints <- function(){
  pattern <- paste0('^mb_albums_(search|lookup)_(\\d+)\\.rds$')
  files <- list.files()
  matching_files <- grep(pattern, files, value = TRUE)
  for(file in matching_files){file.remove(file)}
}
