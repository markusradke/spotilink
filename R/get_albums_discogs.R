#' Get \emph{Discogs} Album Genres and Style
#'
#'
#'
#' @param input
#' Data Frame containing the following columns:
#'\itemize{
#'  \item \code{album.s.id} \cr
#'  with \emph{Spotify} album id,
#'  \item \code{album.s.title} \cr
#'  with \emph{Spotify} album title,
#'  \item \code{track.s.firstartist.name} \cr
#'  with \emph{Spotify} artist name.
#'}
#'It is advisable to first run \code{\link{get_all_spotify}} before running this command,
#'in order to have all the necessary information.
#'
#' @param dc_pass Discogs Authentification. Please refer to \url{https://www.discogs.com/developers/#page:authentication,header:authentication-discogs-auth-flow} for details.
#' @param threshold Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
#'The values correspond to the string similarity (1 - Jaro-Winkler distance) between the searched artist / album and the found name / title on \emph{Spotify}. \emph{spotilink} will only keep results where the artist name as well as the album title surpass the threshold.
#'
#' @return
#' @export
#'
#' @examples
get_albums_discogs <- function(input, dc_pass, album_threshold = 0.8, artist_threshold = 0.8){
  are_needed_columns_present(input, c('album.s.id', 'album.s.title', 'track.s.firstartist.name'))
  input <- rename_existing_variables(input, discogsAlbumVars)

  distinct_input <- dplyr::distinct(input, album.s.id, .keep_all = TRUE)
  res <- purrr::pmap_df(list(distinct_input$album.s.id, distinct_input$album.s.title,
                             distinct_input$track.s.firstartist.name),
                        get_discogs_for_single_track, dc_pass,
                        .progress = 'Linking DC album genres...')
  message('Done.')
  res <- filter_quality_discogs_albums(res, album_threshold, artist_threshold)
  res <- suppressMessages(dplyr::left_join(input, res))
  print_linkage_for_id('album.dc.id', res)
  res
}

get_discogs_for_single_track <- function(album.s.id, album.s.title,track.s.firstartist.name, dc_pass){
  .build_search_url <- function(){
    title <- simplify_name(album.s.title)
    artist <- simplify_name(track.s.firstartist.name)
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
    topresults <- data.frame(id = sapply(res, function(hit) hit$id),
                             title = sapply(res, function(hit) hit$title))
    topresults$genre = lapply(res, function(hit) hit$genre)
    topresults$style = lapply(res, function(hit) hit$style)

    topresults %>%
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
                    track.s.firstartist.name = track.s.firstartist.name) %>%
        tidyr::hoist(title_parts, artist.dc.name = 1L, .remove = F) %>%
        tidyr::hoist(title_parts, album.dc.title = 2L, .remove = F)
  }

  .get_quality_and_best_result <- function(res){
    res %>%
      dplyr::mutate(album.dc.quality = 1 - stringdist::stringdist(simplify_name(album.s.title),
                                                                  simplify_name(album.dc.title),
                                                                  'jw'),
                    artist.dc.quality = 1 - stringdist::stringdist(simplify_name(track.s.firstartist.name),
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
  res <- get_api_with_connection_management(url)
  if(length(res$results) == 0) {return(.make_empty_frame())}
  res <- .parse_results(res)
  res$album.s.id <- album.s.id
  Sys.sleep(1) # Discogs rate limit for authenticated requests is 60 per minute
  res
}
