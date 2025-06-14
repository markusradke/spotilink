#' \emph{Genius} translation retrieval
#'
#' @param input
#' Data Frame containing the following columns:
#'\itemize{
#'  \item \code{track.g.id} \cr
#'  with \emph{Genius} track id,
#'
#'It is advisable to first run \code{\link{get_tracks_genius}} before running this command,
#'in order to have all the necessary information.
#' @param g_token Genius Authentification. Please refer to \url{https://genius.com/api-clients} for generating an access token.
#' @param language Genius language code for the desired translation (e.g., 'en' for English, 'de' for German, 'fr' for French).
#'
#' @returns A new data frame containing the information of the input data frame, as well as new columns with information on the original lyrics language, available translations and a new column containing the version in the desired language, if the version exists.
#' @export
#'
#' @examples
get_translations_genius <- function(input, g_token, language= 'en'){
  are_needed_columns_present(input, c('track.g.id'))
  input_distinct <- input %>%
    dplyr::filter(! is.na(track.g.id)) %>%
    dplyr::distinct(track.g.id, .keep_all = TRUE)


  checkpoint_name <- 'genius_translation'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  saved_data <- checkpoint$saved_data
  if(last_index > 0) {input_distinct <- tail(input_distinct, -last_index)}
  purrr::pmap_df(list(input_distinct$track.g.id,
                      input_distinct$track.g.lyrics),
                 get_translation_for_single_track %>% save_checkpoint_and_count(checkpoint_name,
                                                                                last_index,
                                                                                saved_data,
                                                                                savingstep = 100,
                                                                                ndatapoints = nrow(input_distinct)),
                 g_token,
                 language,
                 .progress = 'Retrieving lyrics translations from Genius...')

  genius <- suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
  message('Done.')


  result <- suppressMessages(dplyr::left_join(input, genius))
  save_file_and_remove_checkpoints(result, checkpoint_name)
  result
}

get_translation_for_single_track <- function(track.g.id, track.g.lyrics, g_token, language){
  res <- get_language_details(track.g.id, g_token)
  res <- copy_lyrics_already_correct_language(res, track.g.lyrics, language)
  is_no_translated_lyrics <- is.na(res$lyrics_translation)
  is_translation_info <- length(res$track.g.translations[[1]]) != 0
  if(is_no_translated_lyrics & is_translation_info){
    available_languages <- get_available_languages(res)
    if(language %in% available_languages$available_language){
      url <- available_languages %>%
        dplyr::filter(language == available_language) %>%
        dplyr::distinct() %>%
        dplyr:: pull(url)
      res$lyrics_translation <- list(retrieve_lyrics_from_url(url))
    }
  }

  res %>% dplyr::rename(!! paste0('track.g.lyrics_', language) := 'lyrics_translation')
}

get_language_details <- function(track.g.id, g_token){
  url <- paste0('https://api.genius.com/songs/', track.g.id, '?access_token=',g_token, '#') %>% utils::URLencode()
  res <- get_api_with_connection_management(url)
  tibble::tibble(track.g.id,
                track.g.language = ifelse(is.null(res$response$song$language), NA, res$response$song$language),
                track.g.translations = ifelse(is.null(list(res$response$song$translation_songs)), NA,
                                              list(res$response$song$translation_songs)))
}

copy_lyrics_already_correct_language <- function(res, track.g.lyrics, language){
  dplyr::mutate(res, lyrics_translation = ifelse(track.g.language == language, list(track.g.lyrics), NA))
}

get_available_languages <- function(input){
  available <- input %>%
    dplyr::mutate(translations = purrr::map(track.g.translations, ~ {
      res <- tibble::tibble(
        available_language = purrr::map_chr(.x, function(x) {
          if (length(x$language) == 0) {
            return(NA)
          } else {
            return(x$language)
          }
        }),
        url = purrr::map_chr(.x, function(x) {
          if (length(x$url) == 0) {
            return(NA)
          } else {
            return(x$url)
          }
        }),
      )
    })) %>%
    tidyr::unnest(translations) %>%
    dplyr::select(available_language, url)
  available
}
