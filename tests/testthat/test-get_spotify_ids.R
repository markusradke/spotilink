test_that('Assertion artist und track vector are charcters and threhshold is a number between 0 and 1', {
  expect_error(get_spotify_ids('Olivia Rodrigo', s_pass, tracks = 1),
               'Please make sure the tracks vector is a character vector without NAs.')
  expect_error(get_spotify_ids(1, s_pass),
               'Please make sure the artists vector is a character vector without NAs.')
  expect_error(get_spotify_ids('Olivia Rodrigo', s_pass, albums = 1),
               'Please make sure the albums vector is a character vector without NAs.')
  expect_error(get_spotify_ids('Olivia Rodrigo', s_pass, tracks = c('vampire', 'drivers license')),
               'Please make sure the artists vector and the optional track / album / release year vectors are all vectors of the same length.')
  expect_error(get_spotify_ids('Olivia Rodrigo', s_pass, albums = c('GUTS', 'SOUR')),
               'Please make sure the artists vector and the optional track / album / release year vectors are all vectors of the same length.')
  expect_error(get_spotify_ids('Olivia Rodrigo', s_pass, albums = c('GUTS', 'SOUR'), releaseyear = 1992),
               'Please make sure the artists vector and the optional track / album / release year vectors are all vectors of the same length.')

  expect_error(get_spotify_ids('Olivia Rodrigo', s_pass, releaseyear = 1992),
               'Please provide a track or album vector when specifiing releaseyears.')

  expect_error(get_spotify_ids('Olivia Rodrigo', s_pass, artist_threshold = -1),
               'Please make sure the threshold is a single number between 0 and 1.')
  expect_error(get_spotify_ids('Olivia Rodrigo', s_pass, artist_threshold = 2),
               'Please make sure the threshold is a single number between 0 and 1.')
  expect_error(get_spotify_ids('Olivia Rodrigo', s_pass, tracks = 'vampire', track_or_album_threshold = c(0.2, 0.1)),
               'Please make sure the threshold is a single number between 0 and 1.')
})

test_that('Returned content is correct', {
  res_artist_only <- suppressMessages(get_spotify_ids(c('Olivia Rodrigo', 'Johann Sebastian Bach'), s_pass))
  expect_setequal(colnames(res_artist_only), c('artist.search', 'artist.s.id', 'artist.s.name', 'artist.s.quality'))
  expect_true(all((res_artist_only$artist.s.quality <= 1 & res_artist_only$artist.s.quality >= 0)| is.na(res_artist_only$artist.s.quality)))
  expect_true(class(res_artist_only$artist.search) == 'character')
  expect_true(class(res_artist_only$artist.s.id) == 'character')
  expect_true(class(res_artist_only$artist.s.name) == 'character')
  expect_true(class(res_artist_only$artist.s.quality) == 'numeric')

  res_track_artist <- suppressMessages(get_spotify_ids(c('Olivia Rodrigo', 'Johann Sebastian Bach'),
                                      s_pass,
                                      tracks = c('vampire', 'Der Geist hilft unserer Schwachheit auf'),
                                      releaseyear = c(2023, 2012)))
  expect_setequal(colnames(res_track_artist), c('artist.search', 'track.s.firstartist.id', 'track.s.firstartist.name', 'track.s.firstartist.quality',
                                               'track.search', 'track.s.id', 'track.s.title', 'track.s.quality'))
  expect_true(all((res_track_artist$track.s.firstartist.quality <= 1 & res_track_artist$track.s.firstartist.quality >= 0) | is.na(res_track_artist$track.s.firstartist.quality)))
  expect_true(all((res_track_artist$track.s.quality <= 1 & res_track_artist$track.s.quality >= 0)| is.na(res_track_artist$track.s.firstartist.quality)))
  expect_true(class(res_track_artist$artist.search) == 'character')
  expect_true(class(res_track_artist$track.s.firstartist.id) == 'character')
  expect_true(class(res_track_artist$track.s.firstartist.name) == 'character')
  expect_true(class(res_track_artist$track.s.firstartist.quality) == 'numeric')
  expect_true(class(res_track_artist$track.search) == 'character')
  expect_true(class(res_track_artist$track.s.id) == 'character')
  expect_true(class(res_track_artist$track.s.title) == 'character')
  expect_true(class(res_track_artist$track.s.quality) == 'numeric')

  res_album_artist <- suppressMessages(get_spotify_ids(c('Olivia Rodrigo', 'Johann Sebastian Bach'),
                                      s_pass,
                                      albums = c('GUTS', 'Bach: Motets')))
  expect_setequal(colnames(res_album_artist), c('artist.search', 'album.s.firstartist.id', 'album.s.firstartist.name', 'album.s.firstartist.quality',
                                               'album.search', 'album.s.id', 'album.s.title', 'album.s.quality'))
  expect_true(all((res_album_artist$album.s.firstartist.quality <= 1 & res_album_artist$album.s.firstartist.quality >= 0) | is.na(res_album_artist$album.s.firstartist.quality)))
  expect_true(all((res_album_artist$album.s.quality <= 1 & res_album_artist$album.s.quality >= 0) | is.na(res_album_artist$album.s.firstartist.quality)))
  expect_true(class(res_album_artist$artist.search) == 'character')
  expect_true(class(res_album_artist$album.s.firstartist.id) == 'character')
  expect_true(class(res_album_artist$album.s.firstartist.name) == 'character')
  expect_true(class(res_album_artist$album.s.firstartist.quality) == 'numeric')
  expect_true(class(res_album_artist$album.search) == 'character')
  expect_true(class(res_album_artist$album.s.id) == 'character')
  expect_true(class(res_album_artist$album.s.title) == 'character')
  expect_true(class(res_album_artist$album.s.quality) == 'numeric')
})


# takes longer to perform
# test_that('function also works for larger amounts of data',{
# (test <- get_spotify_ids(testTracksArtistsAlbums_larger$track.s.firstartist.s.name,
#                  tracks = testTracksArtistsAlbums_larger$track.s.title,
#                  pass = s_pass))
#   expect_true(T)
# })
