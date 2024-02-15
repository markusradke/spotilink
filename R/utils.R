rename_existing_variables <- function(input, database) {
  if(database == 'spotify'){databaseVars <- spotifyVars}
  if(database == 'musicbrainz'){databaseVars <- musicbrainzVars}
  alreadyExistingCols <- colnames(input)[colnames(input) %in% databaseVars]
  new_names <- paste0(alreadyExistingCols, '_old')
  if(length(alreadyExistingCols > 0)) {
      cat(paste0('Renamed existing column for consistency (appended "_old"): ', alreadyExistingCols,'\n'))
  }
  input %>% dplyr::rename_at(dplyr::vars(dplyr::all_of(alreadyExistingCols)), ~new_names)
}
