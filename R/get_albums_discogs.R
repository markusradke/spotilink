#' Get Discogs Album Genres and Style
#'
#' @param input
#' @param dc_pass
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
get_albums_discogs <- function(input, dc_pass, threshold = 0.8){
  check_assertions(input, threshold)
  dc_vars <- c('album.dc.id', 'album.dc.name', 'album.dc.genres', 'album.dc.styles',
               'album.dc.firstgenre', 'album.dc.style',
                'album.dc.quality', 'artist.dc.id', 'artist.dc.name', 'artist.dc.quality')
  rename_existing_variables(input, dc_vars)


}

check_assertions <- function(input, threshold){
  are_needed_columns_present(input, c('album.s.id', 'album.s.title', 'artist.s.name'))
}

get_discogs_for_single_track <- function(album.s.id, album.s.title,artist.s.name, dc_pass){
  .build_search_url <- function(){
    title <- simplify_name(album.s.title)
    artist <- simplify_name(artist.s.name)
    url <- paste0('https://api.discogs.com/database/search?q=',
                  URLencode(title,reserved=T),
                  '&type=track&artist=',
                  URLencode(artist,reserved=T),
                  '&page=1&per_page=5&',
                  'key=', dc_pass[1],"&secret=", dc_pass[2])
    url
  }

  url <- .build_search_url()
  res <- jsonlite::fromJSON(url)
  res
}

# erg <- get_discogs_for_single_track('id', 'get him back', 'Olivia Rodrigo', dc_passport)
# erg

