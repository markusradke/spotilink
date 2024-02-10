get_albums_spotify <- function(input, pass) {
  connect_spotify(pass)
  # res <- rename_existing_variables(input, 'spotify_albums') #TODO implement single renaming requests
  res <- input
  get_from_API(res, 'album.s.id', spotifyr::get_albums, clean_albums, batchsize = 20)
}

clean_albums <- function(albumsRaw){
  albumsRaw %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(album.s.releaseyear = stringr::str_sub(.data[['release_date']], end = 4) %>% as.integer()) %>%
    dplyr::mutate(album.s.releasedate = ifelse(stringr::str_length(.data[['release_date']]) == 4, paste0(.data[['release_date']], '-01-01'),.data[['release_date']])) %>%
    dplyr::mutate(album.s.releasedate = .data[['album.s.releasedate']] %>% as.Date()) %>%
    dplyr::select('album.s.id' = 'id',
                  'album.s.type' = 'type',
                  'album.s.upc' = 'external_ids.upc',
                  'album.s.totaltracks' = 'total_tracks',
                  'album.s.releasedate',
                  'album.s.releaseyear',
                  'album.s.label' = 'label',
                  'album.s.popularity' = 'popularity')
}
