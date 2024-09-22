get_example <- function(){
  data <- dplyr::select(testTracksArtistsAlbums, album.s.id, album.s.title, artist.s.name, track.s.title)
  invisible(capture.output(res <- get_albums_spotify(data, s_pass)))
  invisible(capture.output(res <- get_albums_musicbrainz(res)))
  res <- dplyr::select(res, -album.s.title_old, -artist.s.name, -track.s.title)
  res
}

cat('\nRetrieving test example...\n')
data <-  get_example()

test_that("Are variables complete?", {
  allVariablesLookup <- c(spotifyAlbumVars, musicbrainzAlbumVars)

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
