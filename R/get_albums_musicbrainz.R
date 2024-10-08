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
  res <- input %>%
    dplyr::filter(! is.na(album.s.id)) %>%
    dplyr::distinct(.data[['album.s.id']], .keep_all = TRUE) %>%
    retrieve_albums()
  res %>%
    retrieve_album_genres() %>%
    dplyr::left_join(input, ., by = c('album.s.id')) %>%
    filter_quality_musicbrainz_albums(album_threshold, artist_threshold)
}

retrieve_albums <- function(distinctinput){
  mbAlbums <- c()
  upcCounter <<- 0L
  n <- nrow(distinctinput)
  cat('---------------------------------------------------\n')
  cat('Looking for albums in Musicbrainz...\n')
  for (i in 1:nrow(distinctinput)){
    cat('---------------------------------------------------\n')
    cat('album', i, 'of', n, '\n')
    result <- find_album_with_UPC(distinctinput[i,])
    if  (nrow(result) == 0) {
      result <- find_album_without_UPC(distinctinput[i,])
    }
    mbAlbums <- rbind(mbAlbums, result)
  }
  cat('---------------------------------------------------\n')
  cat(paste0(round(upcCounter / nrow(distinctinput),4) * 100, '% of albums were found using the UPC.\n'))
  rm(upcCounter, pos = .GlobalEnv)
  mbAlbums <- mbAlbums %>%
    dplyr::select('album.mb.id' = 'mbid',
                  'album.mb.title' = 'title',
                  'album.mb.language' = 'language',
                  'album.mb.firstartist.id',
                  'album.mb.firstartist.name',
                  'album.mb.firstartist.quality',
                  'album.mb.quality')
  cbind(album.s.id = distinctinput$album.s.id, mbAlbums)
}

find_album_with_UPC <- function(observation) {
  result <- musicbrainz::search_releases(paste0('barcode:', observation$album.s.upc))
  if (nrow(result) != 0){
    cat('found via UPC\n')
    upcCounter <<- upcCounter + 1
    result <- result[1,] %>%
      tidyr::hoist('artists', album.mb.firstartist.id = list('artist_mbid', 1L), .remove = FALSE) %>%
      tidyr::hoist('artists', album.mb.firstartist.name = list('name', 1L), .remove = FALSE) %>%
      dplyr::mutate(album.mb.quality = 1, album.mb.firstartist.quality = 1)
  }
  result
}

find_album_without_UPC <- function(observation) {
  cat('no UPC, searching...\n')
  musicbrainz::search_releases(paste0('artist:', observation$album.s.firstartist.name,' and release:', observation$album.s.title)) %>%
    tidyr::hoist('artists', album.mb.firstartist.id = list('artist_mbid', 1L), .remove = FALSE) %>%
    tidyr::hoist('artists', album.mb.firstartist.name = list('name', 1L), .remove = FALSE) %>%
    dplyr::mutate(album.mb.quality = stringdist::stringsim(observation$album.s.title %>% simplify_name(),
                                                           title%>% simplify_name(),
                                                           'jw'),
                  album.mb.firstartist.quality = stringdist::stringsim(observation$album.s.firstartist.name %>% simplify_name(),
                                                                       album.mb.firstartist.name %>% simplify_name(),
                                                                       'jw'),
                  album.mb.releaseyear = stringr::str_sub(date, 1, 4) %>% as.integer(),
                  releasediff = abs(observation$album.s.releaseyear - album.mb.releaseyear)) %>%
    dplyr::arrange(-album.mb.firstartist.quality, -album.mb.quality, releasediff, -score) %>%
    dplyr::first() %>%
    dplyr::select(-releasediff, -album.mb.releaseyear) %>%
    dplyr::mutate(album.mb.quality = calculate_and_print_quality(search = observation$album.s.title,
                                                                 found = .data[['title']]))
}

retrieve_album_genres <- function(albums){
  cat('Looking up album genre...\n')
  albumGenres <- purrr::map_df(albums$album.mb.id, lookup_musicbrainz_album_tags_from_ID, .progress = TRUE) %>%
    get_highest_ranking_genre() %>%
    dplyr::rename('album.mb.genres' = 'genres',
                  'album.mb.topgenre' = 'topgenre') %>%
    dplyr::mutate(album.mb.topgenre = .data[['album.mb.topgenre']] %>% as.character())
  cbind(albums, albumGenres)
}


lookup_musicbrainz_album_tags_from_ID  <- function(mbID) {
  res <- musicbrainz::lookup_release_by_id(mbID, includes=c('tags'))
  suppressWarnings(dplyr::mutate(res, score = .data[['score']] %>% as.character()))
}
