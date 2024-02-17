get_single_example_album <- function(){
  data <-  data.frame(album.s.id = '1xJHno7SmdVtZAtXbdbDZp')
  pass <- c("bf4b7a7cffc547d49199cab4ae0b347f","5fe2a814df864abda82b740ecc307661")
  invisible(capture.output(res <- get_albums_spotify(data, pass)))
  # invisible(capture.output(res <- get_all_musicbrainz(res)))
  res
}

test_that("Are variables complete?", {
  data <-  get_single_example_album()

  allVariablesLookup <- c(
    'album.s.id',
    'album.s.title',
    'album.s.type',
    'album.s.upc',
    'album.s.totaltracks',
    'album.s.releasedate',
    'album.s.releaseyear',
    'album.s.label',
    'album.s.popularity'
    # 'album.mb.id',
    # 'album.mb.title',
    # 'album.mb.quality',
    # 'album.mb.genres',
    # 'album.mb.topgenre',
    # 'album.mb.combinedgenre'
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
