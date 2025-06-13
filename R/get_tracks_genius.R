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
#' @param track_threshold,firstartist_threshold
#' Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
#'The values correspond to the string similarity (1 - Jaro-Winkler distance) between the searched artist / track and the found name / title on \emph{Spotify}. \emph{spotilink} will only keep results where the artist name as well as the album title surpass the threshold.
#'
#' @return
#' @export
#'
#' @examples
get_tracks_genius <- function(input, g_token, track_threshold = 0.8, firstartist_threshold = 0.8){
  are_needed_columns_present(input, c('track.s.id', 'track.s.title', 'track.s.firstartist.name'))
  input <- rename_existing_variables(input, geniusLyricsVars)


  input_distinct <- dplyr::distinct(input, track.s.id, track.s.firstartist.name, .keep_all = T) %>% dplyr::filter(! is.na(track.s.id))

  checkpoint_name <- 'genius'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  saved_data <- checkpoint$saved_data
  if(last_index > 0) {input_distinct <- tail(input_distinct, -last_index)}
  purrr::pmap_df(list(input_distinct$track.s.title,
                                input_distinct$track.s.firstartist.name,
                                input_distinct$track.s.id),
                           get_lyrics_for_single_track %>% save_checkpoint_and_count(checkpoint_name,
                                                                                     last_index,
                                                                                     saved_data,
                                                                                     savingstep = 20,
                                                                                     ndatapoints = nrow(input_distinct)),
                           g_token, .progress = 'Retrieving lyrics from Genius...')

  genius <- suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
  message('Done.')


  genius <- filter_quality_genius_tracks(genius, track_threshold, firstartist_threshold)
  result <- suppressMessages(dplyr::left_join(input, genius))
  print_linkage_for_id('track.g.id', result)
  save_file_and_remove_checkpoints(result, checkpoint_name)
  result
}

get_lyrics_for_single_track <- function(track.s.title, artist.s.name, track.s.id, g_token){
  .get_parsed_topresult <- function(result){
    topresults <- data.frame(track.g.id = sapply(result$response$hits, function(hit) hit$result$id %>% as.character()),
                             track.g.title = sapply(result$response$hits, function(hit) hit$result$title),
                             track.g.firstartist.id = sapply(result$response$hits, function(hit) hit$result$primary_artist$id %>% as.character()),
                             track.g.firstartist.name = sapply(result$response$hits, function(hit) hit$result$primary_artist$name),
                             track.g.lyricsstate = sapply(result$response$hits, function(hit) hit$result$lyrics_state),
                             track.g.url = sapply(result$response$hits, function(hit) hit$result$url))
    if(nrow(topresults) == 0){return(NULL)}
    topresults %>%
      dplyr::mutate(track.g.quality = stringdist::stringsim(track.s.title %>% simplify_name(), track.g.title %>% simplify_name()),
                    track.g.firstartist.quality = stringdist::stringsim(artist.s.name %>% simplify_name(), track.g.firstartist.name %>% simplify_name())) %>%
      dplyr::arrange(-track.g.firstartist.quality, -track.g.quality) %>%
      dplyr::first()
  }

  search_term <- paste(track.s.title %>% simplify_name(),
                       artist.s.name %>% simplify_name())
  url <- paste0('https://api.genius.com/search/?q="', search_term, '"&page=1&access_token=',g_token, '#') %>% utils::URLencode()
  result <- get_api_with_connection_management(url)
  topresult <- .get_parsed_topresult(result)
  if(is.null(topresult)){return(make_na_frame_genius_tracks(track.s.id))}
  lyrics <- retrieve_lyrics_from_url(topresult$track.g.url)
  topresult %>%
    dplyr::mutate(track.g.lyrics = list(lyrics),
                  track.s.id = track.s.id)
}


retrieve_lyrics_from_url <- function(url){
  lyrics_html <- read_html_with_retries(url)
  lyrics <- lyrics_html %>% rvest::html_element(xpath = "//div[contains(@class, 'Lyrics__Container')]")

  header <- xml2::xml_find_first(lyrics, ".//div[(contains(@class, 'LyricsHeader__Container'))]")
  xml2::xml_remove(header)


  xml2::xml_find_all(lyrics, ".//br") %>% xml2::xml_add_sibling("p", "\n")
  xml2::xml_find_all(lyrics, ".//br") %>% xml2::xml_remove()
  lyrics <- rvest::html_text(lyrics, trim = TRUE)
  lyrics <- unlist(strsplit(lyrics, split = "\n"))
  lyrics <- grep(pattern = "[[:alnum:]]", lyrics, value = TRUE)
  if (sjmisc::is_empty(lyrics)) {
    return(data.frame(line = NA, section_name = NA))
  }
  section_tags <- nchar(gsub(pattern = "\\[.*\\]", "", lyrics)) == 0
  sections <- repeat_before(lyrics, section_tags)
  sections <- gsub("\\[|\\]", "", sections)
  sections <- strsplit(sections, split = ": ", fixed = TRUE)
  section <- sapply(sections, "[", 1)
  lyrics_res <- data.frame(line = lyrics[!section_tags], section = section[!section_tags])
  if (length(unique(lyrics_res$section)) == 1) {
    lyrics_res$section <- NA
  }
  lyrics_res
}

read_html_with_retries <- function(url, max_attempts = 10, wait_time = 10) {
  for (attempt in 1:max_attempts) {
    tryCatch({
      html_content <- rvest::read_html(url)
      return(html_content)
    }, error = function(e) {
      message(sprintf("Attempt %d failed: %s. Retrying in %d seconds...",
                      attempt, e$message, wait_time))
      Sys.sleep(wait_time)
    })
  }
  message(sprintf("Failed to read HTML from %s after %d attempts.", url, max_attempts))
  return(NULL)
}

repeat_before <- function (x, y){
  ind = which(y)
  if (!y[1])
    ind = c(1, ind)
  rep(x[ind], times = diff(c(ind, length(x) + 1)))
}

