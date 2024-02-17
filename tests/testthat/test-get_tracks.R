get_single_example_song <- function(){
  data <-  data.frame(track.s.id = '7iN1s7xHE4ifF5povM6A48')
  pass <- c("bf4b7a7cffc547d49199cab4ae0b347f","5fe2a814df864abda82b740ecc307661")
  invisible(capture.output(res <- get_tracks_spotify(data, pass)))
  # invisible(capture.output(res <- get_all_musicbrainz(res)))
  res
}

test_that("Are variables complete?", {
  data <-  get_single_example_song()

  allVariablesLookup <- c(
    'track.s.id',
    'track.s.title',
    'track.s.artists',
    'track.s.firstartist.id',
    'track.s.firstartist.name',
    'track.s.explicit',
    'track.s.popularity',
    'track.s.isrc',
    'track.s.duration',
    'track.s.durationms',
    'track.s.albumposition',
    'track.s.danceability',
    'track.s.energy',
    'track.s.key',
    'track.s.loudness',
    'track.s.mode',
    'track.s.speechiness',
    'track.s.acousticness',
    'track.s.instrumentalness',
    'track.s.liveness',
    'track.s.valence',
    'track.s.tempo',
    'track.s.timesignature',
    'album.s.id',
    'album.s.title'
    # 'track.mb.id',
    # 'track.mb.title',
    # 'track.mb.quality',
    # 'track.mb.artistlist',
    # 'track.mb.firstartist.id',
    # 'track.mb.firstartist.name',
    # 'track.mb.releases',
    # 'track.mb.genres',
    # 'track.mb.topgenre',
    # 'track.mb.combinedgenre'
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
