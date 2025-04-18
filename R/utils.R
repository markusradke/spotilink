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
      if(stringr::str_detect(response$url, 'spotify')){
        retry_after <- response$headers$`retry-after` %>% as.integer() + 1
        message(paste0('Rate limit exceeded. Waiting for ', retry_after, ' seconds, then trying again...'))
        Sys.sleep(retry_after)
      }
      else{
        message('Rate limit exceeded. Waiting for 45 seconds, then trying again...')
        Sys.sleep(45)
      }
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

  freq_na <- nrow(dplyr::filter(frame, ! is.na(.data[[idcol]])))
  relfreq_na <- freq_na / nrow(frame)
  relfreq_na_percent <- 100 * round(relfreq_na, 4)
  freq_na_distinct <- nrow(dplyr::filter(frame_distinct, ! is.na(.data[[idcol]])))
  relfreq_na_distinct <- freq_na_distinct / nrow(frame_distinct)
  relfreq_na_percent_distinct <- 100 * round(relfreq_na_distinct, 4)


  message(paste0('Found ', relfreq_na_percent_distinct, '% of distinct ', entity, 's in the data set.\n',
                 'This equals to ', relfreq_na_percent, '% of all ', entity, 's in the data set.'))
  data.frame(database = idcol,
             'freq' = freq_na,
             'distfreq' = freq_na_distinct,
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

save_checkpoint_and_count <- function(f, checkpoint_filename, last_index, checkpoint_data, savingstep = 1, ndatapoints = -1) {
  force(f)
  force(checkpoint_filename)
  force(last_index)
  force(checkpoint_data)
  force(ndatapoints)

  if (savingstep > 1 & ndatapoints == -1) {stop(paste0('Please provide the number of data points ndatapoints when using a saving stepsize > 1; used savingstep = ', savingstep, ' and ndatapoints = ', ndatapoints, '...'))}
  i <- last_index
  ndatapoints <- last_index + ndatapoints
  old_filename <- suppressMessages(read_checkpoint(checkpoint_filename)$last_checkpoint)
  saved_data <- checkpoint_data

  function(...) {
    i <<- i + 1
    result <- f(...)
    saved_data <<- dplyr::bind_rows(saved_data, result)
    if(i %% savingstep == 0 | i == ndatapoints) {
      current_filename <- paste0(checkpoint_filename, '_', i, '.rds')
      saveRDS(saved_data, current_filename)
      if (file.exists(old_filename)){
        file.remove(old_filename)
      }
      old_filename <<- current_filename
    }
    result
  }
}

read_checkpoint <- function(checkpointfilename){
  pattern <- paste0('^', checkpointfilename, '_(\\d+)\\.rds$')
  files <- list.files()
  matching_files <- grep(pattern, files, value = TRUE)

  if (length(matching_files) > 0) {
    last_index <- sub(pattern, '\\1', matching_files) %>% as.integer() %>% max()
    message(paste0('Detected checkpoint. Continuing with index ', last_index + 1, '...'))
    last_checkpoint <- paste0(checkpointfilename, '_', last_index, '.rds')
    saved_data <- readRDS(last_checkpoint)
    return(list(last_index = last_index, saved_data = saved_data, last_checkpoint = last_checkpoint))
  }
  else {
    return(list(last_index = 0, saved_data = c(), last_checkpoint = ''))
  }
}

save_file_and_remove_checkpoints <- function(result, checkpointfilename) {
  saveRDS(result, paste0(checkpointfilename, '.rds'))

  pattern <- paste0('^', checkpointfilename, '_(\\d+)\\.rds$')
  files <- list.files()
  matching_files <- grep(pattern, files, value = TRUE)
  for(file in matching_files){
    file.remove(file)
  }
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
               'freq' = n_complete,
               'distfreq' = n_complete_distinct,
               'relfreq' = perc_complete,
               'distrelfreq' = perc_complete_distinct)
  }

  idvecs <- colnames(frame)[colnames(frame) %in% c('track.s.id',
                                                   'album.s.id',
                                                   'artist.s.id',
                                                   'track.mb.id',
                                                   'album.mb.id',
                                                   'artist.mb.id',
                                                   'track.dz.id',
                                                   'album.dc.id',
                                                   'track.g.id',
                                                   'album.dz.id',
                                                   'artist.dz.id',
                                                   'track.ab.id')]
  res <- suppressMessages(purrr::map_df(idvecs, print_linkage_for_id, frame))
  complete <- .get_complete_linkage(frame, idvecs)
  res <- rbind(res, complete)
  dplyr::as_tibble(res)
}
