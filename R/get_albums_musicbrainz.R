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
#'@param threshold
#'Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
#'The values correspond to the string similarity (1 - Jaro-Winkler distance) between the album title on \emph{Spotify} and the found album title on \emph{Musicbrainz}.
#'
#' @return Data Frame with added information from the \emph{MusicBrainz} API using the  \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
#'pass <- c('YOUR CLIENT ID', 'YOUR CLIENT SECRET')
#'data <- data.frame(album.s.id = c('2rlWvQ1GuTOSkyNpmcoaMC'))
#'data <- get_albums_spotify(data, pass)
#'
#'get_albums_musicbrainz(data)
get_albums_musicbrainz <- function(input, threshold = 0.8) {
  are_needed_columns_present(input, c('album.s.id', 'album.s.title', 'album.s.upc'))
  renameVars <- musicbrainzAlbumVars[! musicbrainzAlbumVars %in% c('album.s.id', 'album.s.title', 'album.s.upc')]
  res <- rename_existing_variables(input, renameVars)

  pull_albums_musicbrainz(res, threshold)
}

pull_albums_musicbrainz <- function(input, threshold) {
  res <- input %>%
    dplyr::distinct(.data[['album.s.id']], .keep_all = TRUE) %>%
    retrieve_albums()
  res %>%
    retrieve_album_genres() %>%
    dplyr::left_join(input, ., by = c('album.s.id')) %>%
    filter_low_quality('album', threshold)
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
                  'album.mb.quality')
  cbind(album.s.id = distinctinput$album.s.id, mbAlbums)
}

find_album_with_UPC <- function(observation) {
  result <- musicbrainz::search_releases(paste0('barcode:', observation$album.s.upc))
  if (nrow(result) != 0){
    cat('found via UPC\n')
    upcCounter <<- upcCounter + 1
    result <- result[1,] %>%
      dplyr::mutate(album.mb.quality = 1)
  }
  result
}

find_album_without_UPC <- function(observation) {
  cat('no UPC, searching...\n')
  musicbrainz::search_releases(paste0('artist:', observation$artist.s.name,' and release:', observation$album.s.title)) %>%
    .[1,] %>%
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
  musicbrainz::lookup_release_by_id(mbID, includes=c('tags')) %>%
    dplyr::mutate(score = .data[['score']] %>% as.character())
}
