get_single_example_artist <- function(){
  data <-  data.frame(artist.s.id = '1QL7yTHrdahRMpvNtn6rI2')
  pass <- c("bf4b7a7cffc547d49199cab4ae0b347f","5fe2a814df864abda82b740ecc307661")
  invisible(capture.output(res <- get_artists_spotify(data, pass)))
  res
}

test_that("Artist variables complete?", {
  data <-  get_single_example_artist()

  allVariablesLookup <- c(
    'artist.s.id',
    'artist.s.name',
    'artist.s.genres',
    'artist.s.topgenre',
    'artist.s.popularity',
    'artist.s.followers'
    # 'artist.mb.id',
    # 'artist.mb.name',
    # 'artist.mb.quality',
    # 'artist.mb.type',
    # 'artist.mb.gender',
    # 'artist.mb.origin',
    # 'artist.mb.area',
    # 'artist.mb.birth',
    # 'artist.mb.birthyear',
    # 'artist.mb.death',
    # 'artist.mb.deathyear',
    # 'artist.mb.dead',
    # 'artist.mb.genres',
    # 'artist.mb.topgenre',
    # 'artist.mb.combinedgenre',
  )

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
