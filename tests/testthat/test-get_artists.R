get_example <- function(){
  data <- dplyr::select(testTracksArtistsAlbums, artist.s.id, artist.s.name, track.s.title)
  invisible(capture.output(res <- get_artists_spotify(data, s_pass)))
  invisible(capture.output(res <- get_artists_musicbrainz(res)))
  res <- dplyr::select(res, -track.s.title, - artist.s.name_old)
  res
}

cat('\nRetrieving test example...\n')
data <-  get_example()

test_that("Artist variables complete?", {
  allVariablesLookup <- c(spotifyArtistVars, musicbrainzArtistVars)

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


test_that('Retrieve Number of Artists > 50 from Spotify (70)',{
  data <- largeArtistTest
  invisible(capture.output(res <- get_artists_spotify(data, s_pass)))
  expect_equal(
    nrow(res), 70
  )
})

