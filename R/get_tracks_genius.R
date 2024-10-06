#' Get \emph{Genius} song lyrics
#'
#' @param input
#' Data Frame containing the following columns:
#'\itemize{
#'  \item \code{track.s.id} \cr
#'  with \emph{Spotify} track id,
#'  \item \code{track.s.title} \cr
#'  with \emph{Spotify} track title,
#'  \item \code{artist.s.name} \cr
#'  with \emph{Spotify} artist name.
#'}
#'It is advisable to first run \code{\link{get_all_spotify}} before running this command,
#'in order to have all the necessary information.
#' @param g_token Genius Authentification. Please refer to \url{https://genius.com/api-clients} for generating an access token.
#' @param threshold Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
#'The values correspond to the string similarity (1 - Jaro-Winkler distance) between the searched artist / album and the found name / title on \emph{Spotify}. \emph{spotilink} will only keep results where the artist name as well as the album title surpass the threshold.
#'
#' @return
#' @export
#'
#' @examples
get_tracks_genius <- function(input, g_token, track_threshold = 0.8, artist_threshold = 0.8){
  are_needed_columns_present(input, c('track.s.id', 'track.s.title', 'track.s.firstartist.name'))
  input <- rename_existing_variables(input, geniusLyricsVars)

  input_distinct <- dplyr::distinct(input, track.s.id, track.s.firstartist.name, .keep_all = T)
  genius <- purrr::pmap_df(list(input_distinct$track.s.title,
                                input_distinct$track.s.firstartist.name,
                                input_distinct$track.s.id),
                           get_lyrics_for_single_track, g_token, .progress = 'Retrieving lyrics from Genius...')
  message('Done.')
  genius <- filter_quality_genius_tracks(genius, track_threshold, artist_threshold)
  result <- suppressMessages(dplyr::left_join(input, genius))
  print_linkage_for_id(result, 'track.g.id')
  result
}

get_lyrics_for_single_track <- function(track.s.title, artist.s.name, track.s.id, g_token){
  .get_parsed_topresult <- function(result){
    topresults <- data.frame(track.g.id = sapply(result$response$hits, function(hit) hit$result$id %>% as.character()),
                             track.g.title = sapply(result$response$hits, function(hit) hit$result$title),
                             artist.g.id = sapply(result$response$hits, function(hit) hit$result$primary_artist$id %>% as.character()),
                             artist.g.name = sapply(result$response$hits, function(hit) hit$result$primary_artist$name),
                             track.g.lyricsstate = sapply(result$response$hits, function(hit) hit$result$lyrics_state),
                             track.g.url = sapply(result$response$hits, function(hit) hit$result$url))
    if(nrow(topresults) == 0){return(NULL)}
    topresults %>%
      dplyr::mutate(track.g.quality = stringdist::stringsim(track.s.title %>% simplify_name(), track.g.title %>% simplify_name()),
                    artist.g.quality = stringdist::stringsim(artist.s.name %>% simplify_name(), artist.g.name %>% simplify_name())) %>%
      dplyr::arrange(-artist.g.quality, -track.g.quality) %>%
      dplyr::first()
  }

  .get_lyrics_for_topresult <- function(topresult){
    lyrics_html <- rvest::read_html(topresult$track.g.url)
    lyrics <- lyrics_html %>% rvest::html_nodes(xpath = "//div[contains(@class, \"Lyrics__Container\")]")
    xml2::xml_find_all(lyrics, ".//br") %>% xml2::xml_add_sibling("p", "\n")
    xml2::xml_find_all(lyrics, ".//br") %>% xml2::xml_remove()
    lyrics <- rvest::html_text(lyrics, trim = TRUE)
    lyrics <- unlist(strsplit(lyrics, split = "\n"))
    lyrics <- grep(pattern = "[[:alnum:]]", lyrics, value = TRUE)
    if (sjmisc::is_empty(lyrics)) {
      return(data.frame(line = NA, section_name = NA))
    }
    section_tags <- nchar(gsub(pattern = "\\[.*\\]", "", lyrics)) == 0
    sections <- .repeat_before(lyrics, section_tags)
    sections <- gsub("\\[|\\]", "", sections)
    sections <- strsplit(sections, split = ": ", fixed = TRUE)
    section <- sapply(sections, "[", 1)
    data.frame(line = lyrics[!section_tags], section = section[!section_tags])

  }

  .repeat_before <- function (x, y){
     ind = which(y)
    if (!y[1])
      ind = c(1, ind)
    rep(x[ind], times = diff(c(ind, length(x) + 1)))
  }

  search_term <- paste(track.s.title %>% simplify_name(),
                       artist.s.name %>% simplify_name())
  url <- paste0('https://api.genius.com/search/?q="', search_term, '"&page=1&access_token=',g_token, '#') %>% utils::URLencode()
  result <- get_api_with_connection_management(url)
  topresult <- .get_parsed_topresult(result)
  if(is.null(topresult)){return(make_na_frame_genius_tracks(track.s.id))}
  lyrics <- .get_lyrics_for_topresult(topresult)
  topresult %>%
    dplyr::mutate(track.g.lyrics = list(lyrics),
                  track.s.id = track.s.id) %>%
    dplyr::select(-track.g.url)
}

