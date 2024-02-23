rename_existing_variables <- function(input, variableSet) {
  alreadyExistingCols <- colnames(input)[colnames(input) %in% variableSet]
  new_names <- paste0(alreadyExistingCols, '_old')
  if(length(alreadyExistingCols > 0)) {
      cat(paste0('Renamed existing column for consistency (appended "_old"): ', alreadyExistingCols,'\n'))
  }
  input %>% dplyr::rename_at(dplyr::vars(dplyr::all_of(alreadyExistingCols)), ~new_names)
}

simplify_name <- function(name) {
  name %>%
    stringr::str_replace(pattern = ' \\(.*', replacement = '') %>%
    stringr::str_replace(pattern = ' -.*', replacement = '') %>%
    toupper()
}
