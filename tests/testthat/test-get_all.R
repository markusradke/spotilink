get_example <- function(){
  data <- dplyr::select(testTracksArtistsAlbums, -track.s.keyconfidence)
  invisible(capture.output(res <- get_all_spotify(data, s_pass)))
  invisible(capture.output(res <- get_all_musicbrainz(res)))
  res <- dplyr::select(res, -track.s.title_old, -album.s.id_old, -album.s.title_old, -artist.s.id_old, -artist.s.name_old)
  res
}

cat('\nRetrieving test example...\n')
data <-  get_example()

test_that("Are variables complete?", {
  allVariablesLookup <- c(spotifyAllVars, musicbrainzAllVars)

  columns <- names(data)
  missingVariables <- allVariablesLookup[!allVariablesLookup %in% columns]
  superfluousVariables <- columns[! columns %in% allVariablesLookup]

  names(missingVariables) <- rep('missing', length(missingVariables))
  names(superfluousVariables) <- rep('superflous', length(superfluousVariables))

  print(missingVariables)
  print(superfluousVariables)

  test <- c(missingVariables, superfluousVariables)

  expect_equal(
    length(test) == 0,
    TRUE)
})


test_that('Variable Types Correct', {
  actualTypes <- data %>%
    purrr::map_vec(class)

  correctTypesLookup <- correctTypesAll %>% .[names(actualTypes)]

  test <- correctTypesLookup == actualTypes

  if (! all(test)) {
    cat('There are unwanted types: \n')
    print(actualTypes[! test])
  }

  expect_equal(
    all(test),
    TRUE)
})
