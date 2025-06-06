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
