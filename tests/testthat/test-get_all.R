get_single_example_song <- function(){
  data <-  data.frame(track.s.id = '7iN1s7xHE4ifF5povM6A48')
  pass <- c("bf4b7a7cffc547d49199cab4ae0b347f","5fe2a814df864abda82b740ecc307661")
  invisible(capture.output(res <- get_all_spotify(data, pass)))
  invisible(capture.output(res <- get_all_musicbrainz(res)))
  res
}

test_that("Are variables complete?", {
  data <-  get_single_example_song()

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
  data <-  get_single_example_song()

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
