rename_existing_variables <- function(input, variableSet) {
  alreadyExistingCols <- colnames(input)[colnames(input) %in% variableSet]
  new_names <- paste0(alreadyExistingCols, '_old')
  if(length(alreadyExistingCols > 0)) {
      message(paste0('Renamed existing column for consistency (appended "_old"): ', alreadyExistingCols,'\n'))
  }
  input %>% dplyr::rename_at(dplyr::vars(dplyr::all_of(alreadyExistingCols)), ~new_names)
}

simplify_name <- function(name) {
  name %>%
    toupper() %>%
    stringr::str_replace(pattern = ' \\(.*', replacement = '') %>%
    stringr::str_replace(pattern = ' -.*', replacement = '') %>%
    stringr::str_replace(pattern = 'FEAT.*', replacement = '') %>%
    stringr::str_replace(pattern = ' &.*', replacement = '') %>%
    stringr::str_replace(pattern = ' FT.*', replacement = '') %>%
    stringr::str_replace(pattern = ':.*', replacement = '')
}

are_needed_columns_present <- function(input, neededCols)
if(! all(neededCols %in% colnames(input))) {
  stop('Please provide a data frame containing the following columns:\n', paste(toString(neededCols), collapse = ", "),'\nSee the function reference for further information.')
}


get_api_with_connection_management <- function(url){
  repeat {
    response <- httr::GET(url)
    if (httr::status_code(response) == 200) {
      res <- httr::content(response)
      return(res)
    } else if (httr::status_code(response) == 429) {
      message('Rate limit exceeded. Waiting for 45 seconds, then trying again...')
      Sys.sleep(45)
    } else {
      message('An error occurred: ', httr::status_code(response), ' - ', suppressMessages(httr::content(response, 'text')))
      return(NULL)
    }
  }
}


print_linkage_for_id <- function(idcol, frame){
  message(paste0('Showing linkage for ', idcol), ':')
  entity <- stringr::str_extract(idcol, '(album|track|artist)')
  frame_distinct <- dplyr::distinct(frame, .data[[paste0(entity, '.s.id')]], .keep_all = T)

  relfreq_na <- nrow(dplyr::filter(frame, ! is.na(.data[[idcol]]))) / nrow(frame)
  relfreq_na_percent <- 100 * round(relfreq_na, 4)
  relfreq_na_distinct <- nrow(dplyr::filter(frame_distinct, ! is.na(.data[[idcol]]))) / nrow(frame_distinct)
  relfreq_na_percent_distinct <- 100 * round(relfreq_na_distinct, 4)

  message(paste0('Found ', relfreq_na_percent_distinct, '% of distinct ', entity, 's in the data set.\n',
                 'This equals to ', relfreq_na_percent, '% of all ', entity, 's in the data set.'))
  data.frame(database = idcol,
             'relfreq' = relfreq_na_percent,
             'distrelfreq' = relfreq_na_percent_distinct)
}


handle_empty_input <- function(input, retrievalfunction, emptyframefunction){
  warning(paste0(retrievalfunction, ' was supplied with an empty input.\n',
                 'Returning empty frame with updated column names.'))

  na_frame <- do.call(emptyframefunction, args = list(NA))
  empty_frame <- na_frame %>% dplyr::filter(! is.na(na_frame[colnames(na_frame)[1], 1]))

  cbind(input, empty_frame)
}

#' Show Linkage Quality
#'
#' Show linkage quality for each database and each quality measure that was calculated available in the input data frame.
#'
#' @param frame Input data frame with linkage results.
#'
#' @return List with plots for each data base for that information is contained in the data frame.
#' @export
#'
#' @examples
#' show_quality_of_linkage_in_frame(testresults)
show_quality_of_linkage_in_frame <- function(frame){
  .plot_histogram_of_quality <- function(qualityvector){
    frame <- frame %>% dplyr::filter(! is.na(.data[[qualityvector]]))
    plot <- ggplot2::ggplot(frame, ggplot2::aes(x = .data[[qualityvector]]))+
    ggplot2::geom_histogram(binwidth = 0.025, fill='white', color='black')+
    ggplot2::xlim(c(-0.025,1.025))
    plot
  }

  .plot_combined_histograms <- function(qualityecs){
    database_plots <- list()
    for(vector in qualityvecs){
      temp_plot <- .plot_histogram_of_quality(vector)
      database_plots <- c(database_plots, list(temp_plot))
    }
    plot <- patchwork::wrap_plots(database_plots, nrow = length(database_plots))
    suppressWarnings(print(plot))
    plot
  }
  plots <- list()
  for(database in c('mb', 'dz', 'g', 'dc')){
    qualityvecs <- colnames(frame) %>%
      stringr::str_subset(paste0(database, '\\..*quality'))
    if(!length(qualityvecs) == 0){
      plot <- .plot_combined_histograms(database)
      plots <- c(plots, list(plot))
    }
  }
  plots
}

#' Show Linkage Success
#'
#' Show linkage success for each database that was linked to \emph{Spotify} with \code{spotilink}. The success calculated for distinct entities within the data is based on the entity that was used searched for on the data base (e.g., for \emph{Discogs} the success is calculated for distinct albums, while it is calculated for distinct tracks for \emph{Genius}). Also returns the relative frequency of complete linkage for tracks.
#'
#' @param frame Input data frame with linkage results.
#'
#' @return Data frame with the relative frequencies of linked entities within the data for all databases that were linked to the original \emph{Spotify} data.
#' @export
#'
#' @examples
#' show_linkage_success_in_frame(testresults)
show_linkage_success_in_frame <- function(frame){
  .get_complete_linkage <- function(frame, idvecs){
    complete <- frame
    for(id in idvecs){
      complete <- complete %>% dplyr::filter(!is.na(.data[[id]]))
    }
    n_complete <- nrow(complete)
    n_complete_distinct <- nrow(complete %>% dplyr::distinct(track.s.id))
    perc_complete <- round(n_complete / nrow(frame), 4) * 100
    perc_complete_distinct <- round(n_complete_distinct / nrow(frame %>% dplyr::distinct(track.s.id)), 4) * 100
    data.frame(database = 'complete track linkage',
               'relfreq' = perc_complete,
               'distrelfreq' = perc_complete_distinct)
  }

  idvecs <- colnames(frame) %>%
    stringr::str_subset('\\.id') %>%
    stringr::str_subset('\\.s\\.', negate = T) %>%
    stringr::str_subset('(firstartist|album\\.(id|firstgenre)|artist\\.g)', negate = T)
  res <- suppressMessages(purrr::map_df(idvecs, print_linkage_for_id, frame))
  complete <- .get_complete_linkage(frame, idvecs)
  res <- rbind(res, complete)
  print(dplyr::as_tibble(res))
  res
}
