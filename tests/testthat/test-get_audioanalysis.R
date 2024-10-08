get_example <- function(){
  data <-  dplyr::select(testTracksArtistsAlbums, track.s.id, track.s.title, track.s.keyconfidence)
  invisible(capture.output(res <- get_audioanalysis_spotify(data, s_pass)))
  res <- dplyr::select(res, -track.s.title, -track.s.keyconfidence_old)
  res
}

cat('\nRetrieving test example...\n')
data <-  get_example()

test_that("Are variables complete?", {

  allVariablesLookup <- spotifyAudioanalysisVars

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

  correctTypesLookup <- correctTypesAudioanalysis %>% .[names(actualTypes)]

  test <- correctTypesLookup == actualTypes

  if (! all(test)) {
    cat('There are unwanted types: \n')
    print(actualTypes[! test])
  }

  expect_equal(
    all(test),
    TRUE)
})
