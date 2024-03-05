get_example <- function(){
  testTracksArtistsAlbums
  pass <- c("bf4b7a7cffc547d49199cab4ae0b347f","5fe2a814df864abda82b740ecc307661")
  data <- dplyr::select(testTracksArtistsAlbums, track.s.id, track.s.title, artist.s.name, album.s.id, album.s.title)
  invisible(capture.output(res <- get_tracks_spotify(data, pass)))
  invisible(capture.output(res <- get_tracks_musicbrainz(res)))
  res <- dplyr::select(res,-track.s.title_old, -artist.s.name, -album.s.id_old, -album.s.title_old)
  res
}

cat('\nRetrieving test example...\n')
data <-  get_example()

test_that("Are variables complete?", {
  allVariablesLookup <- c(spotifyTrackVars,
                          musicbrainzTrackVars)

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
