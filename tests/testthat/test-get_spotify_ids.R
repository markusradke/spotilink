test_that('Assertion artist und track vector are charcters and threhshold is a number between 0 and 1', {
  expect_error(get_spotify_ids('Olivia Rodrigo', s_pass, tracks = 1),
               'Please make sure the artists vector and the optional track vector are character vectors of the same length.')
  expect_error(get_spotify_ids(1, s_pass),
               'Please make sure the artists vector and the optional track vector are character vectors of the same length.')
  expect_error(get_spotify_ids('Olivia Rodrigo', s_pass, c('vampire', 'drivers license')),
               'Please make sure the artists vector and the optional track vector are character vectors of the same length.')

  expect_error(get_spotify_ids('Olivia Rodrigo', s_pass, threshold = -1),
               'Please make sure the threshold is a single number between 0 and 1.')
  expect_error(get_spotify_ids('Olivia Rodrigo', s_pass, threshold = 2),
               'Please make sure the threshold is a single number between 0 and 1.')
  expect_error(get_spotify_ids('Olivia Rodrigo', s_pass, threshold = c(0.2, 0.1)),
               'Please make sure the threshold is a single number between 0 and 1.')
})

test_that('Returned content is correct', {
  res_artist_only <- get_spotify_ids(c('Olivia Rodrigo', 'Johann Sebastian Bach'), s_pass)
  expect_setequal(colnames(res_artist_only), c('artist.search', 'artist.s.id', 'artists.s.name', 'artist.s.quality'))
  expect_true(all(res_artist_only$artist.s.quality <= 1 & res_artist_only$artist.dc.quality >= 0))
  expect_true(class(res_artist_only$artist.search) == 'character')
  expect_true(class(res_artist_only$artist.s.id) == 'character')
  expect_true(class(res_artist_only$artist.s.name) == 'character')
  expect_true(class(res_artist_only$artist.s.quality) == 'numeric')

  res_track_artist <- get_spotify_ids(c('Olivia Rodrigo', 'Johann Sebastian Bach'),
                                      s_pass,
                                      c('vampire', 'Der Geist hilft unserer Schwachheit auf'))
  expect_setequal(colnames(res_track_artist), c('artist.search', 'artist.s.id', 'artists.s.name', 'artist.s.quality',
                                               'track.search', 'track.s.id', 'track.s.title', 'track.s.quality'))
  expect_true(all(res_track_artist$artist.s.quality <= 1 & res_track_artist$artist.dc.quality >= 0))
  expect_true(class(res_track_artist$artist.search) == 'character')
  expect_true(class(res_track_artist$artist.s.id) == 'character')
  expect_true(class(res_track_artist$artist.s.name) == 'character')
  expect_true(class(res_track_artist$artist.s.quality) == 'numeric')
  expect_true(class(res_track_artist$track.search) == 'character')
  expect_true(class(res_track_artist$track.s.id) == 'character')
  expect_true(class(res_track_artist$track.s.title) == 'character')
  expect_true(class(res_track_artist$track.s.quality) == 'numeric')

  # expect_equal(res_track_artist$track.s.title[2] == 'MAXPOPSOLUTION')
})
