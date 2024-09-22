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
  input <- rename_existing_variables(input, dc_vars)

  res <- purrr::pmap_df(list(input$album.s.id, input$album.s.title,
                        input$artist.s.name),
                        get_discogs_for_single_track, dc_pass,
                        .progress = 'Linking DC album genres...')
  dplyr::left_join(input, res)
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
                  '&page=1&per_page=10&',
                  'key=', dc_pass[1],"&secret=", dc_pass[2])
    url
  }

  .parse_results <- function(res){
    res <- res$results
    data.frame(res) %>%
      .extract_albumtitle_and_artistname_from_dctitle() %>%
      .get_quality_and_best_result() %>%
      dplyr::mutate(album.dc.id = as.character(id)) %>%
      dplyr::select(album.dc.id,
                    album.dc.genres = genre,
                    album.dc.styles = style,
                    album.dc.title,
                    album.dc.quality,
                    artist.dc.name,
                    artist.dc.quality) %>%
      tidyr::hoist(album.dc.genres, album.dc.firstgenre = 1L, .remove = F) %>%
      tidyr::hoist(album.dc.styles, album.dc.firststyle = 1L, .remove = F)
  }

  .extract_albumtitle_and_artistname_from_dctitle <- function(res){
    res %>%
      dplyr::mutate(title_parts = stringr::str_split(.data[['title']], ' - '),
                    album.s.title = album.s.title,
                    artist.s.name = artist.s.name) %>%
        tidyr::hoist(title_parts, artist.dc.name = 1L, .remove = F) %>%
        tidyr::hoist(title_parts, album.dc.title = 2L, .remove = F)
  }

  .get_quality_and_best_result <- function(res){
    res %>%
      dplyr::mutate(album.dc.quality = 1 - stringdist::stringdist(simplify_name(album.s.title),
                                                                  simplify_name(album.dc.title),
                                                                  'jw'),
                    artist.dc.quality = 1 - stringdist::stringdist(simplify_name(artist.s.name),
                                                                   simplify_name(artist.dc.name),
                                                                   'jw'),
                    res_number = dplyr::row_number()) %>%
      dplyr::arrange(artist.dc.quality, album.dc.quality, res_number) %>%
      dplyr::first()
  }

  .make_empty_frame <- function(){
    data.frame(
      album.s.id = album.s.id,
      album.dc.id = NA,
      album.dc.genres = NA,
      album.dc.firstgenre = NA,
      album.dc.styles = NA,
      album.dc.firststyle = NA,
      album.dc.title = NA,
      album.dc.quality = NA,
      artist.dc.name = NA,
      artist.dc.quality = NA
    )
  }

  url <- .build_search_url()
  res <- jsonlite::fromJSON(url)
  if(length(res$results) == 0) {return(.make_empty_frame())}
  res <- .parse_results(res)
  res$album.s.id <- album.s.id
  res
}
