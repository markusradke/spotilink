#'Get \emph{Spotify} API Information on Albums
#'
#'Retrieve a data frame containing information from the \emph{Spotify} API.
#'The result contains information on the albums using the  \pkg{spotilink} naming convention.
#'
#' @param input
#'Data Frame containing a column \code{album.s.id} with \emph{Spotify} album ids.
#' @param pass
#'Character Vector containing two entries: \emph{Client ID} and \emph{Client secret}. See \url{https://developer.spotify.com/documentation/web-api/concepts/authorization} for details.
#'
#' @return Data Frame with added information from the \emph{Spotify} Web API using the \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
get_albums_spotify <- function(input, pass) {
  renameVars <- spotifyAlbumVars[! spotifyAlbumVars %in% c('album.s.id')]
  input <- rename_existing_variables(input, renameVars)
  are_needed_columns_present(input, c('album.s.id'))
  input_ready <- input %>% dplyr::filter(!is.na(album.s.id))

  if(nrow(input_ready) == 0){
    res <- handle_empty_input(input_ready, 'get_albums_spotify', 'make_na_frame_spotify_albums')
  }

  connect_spotify(pass)
  res <- pull_albums_spotify(input_ready)

  na_ids <- input %>% dplyr::filter(is.na(album.s.id))
  if(nrow(na_ids) > 0){
    res <- na_ids %>% dplyr::select(-album.s.id) %>%
      cbind(make_na_frame_spotify_albums(NA)) %>%
      rbind(res, .)
  }
  res
}

pull_albums_spotify <- function(input, pass){
  get_from_API(input, 'album.s.id', spotifyr::get_albums, clean_albums, batchsize = 20)
}

clean_albums <- function(albumsRaw){
  albumsRaw %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(album.s.releaseyear = stringr::str_sub(.data[['release_date']], end = 4) %>% as.integer()) %>%
    dplyr::mutate(album.s.releasedate = ifelse(stringr::str_length(.data[['release_date']]) == 4, paste0(.data[['release_date']], '-01-01'),.data[['release_date']])) %>%
    dplyr::mutate(album.s.releasedate = .data[['album.s.releasedate']] %>% as.Date()) %>%
    dplyr::select('album.s.id' = 'id',
                  'album.s.title' = 'name',
                  'album.s.type' = 'type',
                  'album.s.upc' = 'external_ids.upc',
                  'album.s.totaltracks' = 'total_tracks',
                  'album.s.releasedate',
                  'album.s.releaseyear',
                  'album.s.label' = 'label',
                  'album.s.popularity' = 'popularity')
}
