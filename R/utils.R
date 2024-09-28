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
    stringr::str_replace(pattern = ' \\(.*', replacement = '') %>%
    stringr::str_replace(pattern = ' -.*', replacement = '') %>%
    stringr::str_replace(pattern = 'feat.*', replacement = '') %>%
    stringr::str_replace(pattern = ' &.*', replacement = '') %>%
    stringr::str_replace(pattern = ' Ft.*', replacement = '') %>%
    toupper()
}

are_needed_columns_present <- function(input, neededCols)
if(! all(neededCols %in% colnames(input))) {
  stop('Please provide a data frame containing the following columns:\n', paste(toString(neededCols), collapse = ", "),'\nSee the function reference for further information.')
}


get_api_with_connection_management <- function(url){
  repeat {
    response <- httr::GET(url)
    if (httr::status_code(response) == 200) {
      res <- suppressMessages(jsonlite::fromJSON(httr::content(response, 'text')))
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

print_linkage_for_id <- function(frame, idcol){
  entity <- stringr::str_extract(idcol, '(album|track|artist)')
  frame_distinct <- dplyr::distinct(frame, .data[[idcol]], .keep_all = T)

  relfreq_na <- nrow(dplyr::filter(frame, ! is.na(.data[[idcol]]))) / nrow(frame)
  relfreq_na_percent <- 100 * round(relfreq_na, 4)
  relfreq_na_distinct <- nrow(dplyr::filter(frame_distinct, ! is.na(.data[[idcol]]))) / nrow(frame_distinct)
  relfreq_na_percent_distinct <- 100 * round(relfreq_na, 4)

  message(paste0('Found ', relfreq_na_percent, '% of distinct ', entity, 's in the data set.\n',
                 'This equals to ', relfreq_na_percent_distinct, '% of all ', entity, 's in the data set.'))
}

