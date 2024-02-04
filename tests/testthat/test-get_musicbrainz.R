get_single_example_song <- function(){
  data <-  data.frame(track.s.id = '7iN1s7xHE4ifF5povM6A48')
  pass <- c("bf4b7a7cffc547d49199cab4ae0b347f","5fe2a814df864abda82b740ecc307661")
  invisible(capture.output(res <- get_spotify(data, pass)))
  res <- get_musicbrainz(res)
  # invisible(capture.output(res <- get_musicbrainz(res)))
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
    'track.mb.id',
    'track.mb.title',
    'track.mb.quality',
    'track.mb.artistlist',
    'track.mb.firstartist.id',
    'track.mb.firstartist.name',
    'track.mb.releases',
    'track.mb.genres',
    'track.mb.topgenre',
    'track.mb.combinedgenre',
    'artist.s.id',
    'artist.s.name',
    'artist.s.genres',
    'artist.s.topgenre',
    'artist.s.popularity',
    'artist.s.followers',
    'artist.mb.id',
    'artist.mb.name',
    'artist.mb.quality',
    'artist.mb.type',
    'artist.mb.gender',
    'artist.mb.origin',
    'artist.mb.area',
    'artist.mb.birth',
    'artist.mb.birthyear',
    'artist.mb.death',
    'artist.mb.deathyear',
    'artist.mb.dead',
    'artist.mb.genres',
    'artist.mb.topgenre',
    'artist.mb.combinedgenre',
    'album.s.id',
    'album.s.title',
    'album.s.type',
    'album.s.upc',
    'album.s.totaltracks',
    'album.s.releasedate',
    'album.s.releaseyear',
    'album.s.label',
    'album.s.popularity',
    'album.mb.id',
    'album.mb.title',
    'album.mb.quality',
    'album.mb.genres',
    'album.mb.topgenre',
    'album.mb.combinedgenre'
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


test_that('Variable Types Correct', {
  data <-  get_single_example_song()

  actualTypes <- data %>%
    purrr::map_vec(class)

  correctTypesLookup <- c(
    track.s.id	= 'character',
    track.s.title	= 'character',
    track.s.artists	= 'list',
    track.s.firstartist.id	= 'character',
    track.s.firstartist.name	= 'character',
    track.s.explicit = 'logical',
    track.s.popularity	= 'integer',
    track.s.isrc	= 'character',
    track.s.durationms	= 'integer',
    track.s.duration	= 'numeric',
    track.s.albumposition	= 'integer',
    track.s.danceability	= 'numeric',
    track.s.energy	= 'numeric',
    track.s.key	= 'character',
    track.s.loudness	= 'numeric',
    track.s.mode	= 'character',
    track.s.speechiness	= 'numeric',
    track.s.acousticness	= 'numeric',
    track.s.instrumentalness	= 'numeric',
    track.s.liveness	= 'numeric',
    track.s.valence	= 'numeric',
    track.s.tempo	= 'numeric',
    track.s.timesignature	= 'character',
    track.mb.id	= 'character',
    track.mb.title	= 'character',
    track.mb.quality	= 'numeric',
    track.mb.artistlist	= 'list',
    track.mb.firstartist.id = 'character',
    track.mb.firstartist.name = 'character',
    track.mb.releases	= 'list',
    track.mb.genres	= 'list',
    track.mb.topgenre	= 'character',
    track.mb.combinedgenre	= 'character',
    artist.s.id	= 'character',
    artist.s.name	= 'character',
    artist.s.genres	= 'list',
    artist.s.topgenre	= 'character',
    artist.s.popularity	= 'integer',
    artist.s.followers	= 'integer',
    artist.mb.id	= 'character',
    artist.mb.name	= 'character',
    artist.mb.quality	= 'numeric',
    artist.mb.type	= 'character',
    artist.mb.gender	= 'character',
    artist.mb.origin	= 'character',
    artist.mb.area	= 'character',
    artist.mb.birth	= 'Date',
    artist.mb.birthyear	= 'integer',
    artist.mb.death	= 'Date',
    artist.mb.deathyear	= 'integer',
    artist.mb.dead	= 'logical',
    artist.mb.genres	= 'list',
    artist.mb.topgenre	= 'character',
    artist.mb.combinedgenre	= 'character',
    album.s.id	= 'character',
    album.s.title	= 'character',
    album.s.type	= 'character',
    album.s.upc	= 'character',
    album.s.totaltracks	= 'integer',
    album.s.releasedate	= 'Date',
    album.s.releaseyear	= 'integer',
    album.s.label	= 'character',
    album.s.popularity	= 'integer',
    album.mb.id	= 'character',
    album.mb.title	= 'character',
    album.mb.quality	= 'numeric',
    album.mb.genres	= 'list',
    album.mb.topgenre	= 'character',
    album.mb.combinedgenre	= 'character'
  ) %>%
    .[names(actualTypes)]

  test <-  correctTypesLookup == actualTypes

  if (! all(test)) {
    cat('There are unwanted types: \n')
    print(actualTypes[! test])
  }

  expect_equal(
    all(test),
    TRUE)
})
