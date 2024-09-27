test_that('Assertion all variables in input data frame', {
  expect_error(get_tracks_lyrics_genius(data.frame(track.s.id = c('a', 'b'), track.s.title = c('a', 'b')), g_token),
               'Please provide a data frame containing the following columns:\ntrack.s.id, track.s.title, artist.s.name\nSee the function reference for further information.')
  expect_error(get_tracks_lyrics_genius(data.frame(track.s.id = c('a', 'b'), artist.s.name = c('a', 'b')), g_token),
               'Please provide a data frame containing the following columns:\ntrack.s.id, track.s.title, artist.s.name\nSee the function reference for further information.')
  expect_error(get_tracks_lyrics_genius(data.frame(track.s.title = c('a', 'b'), artist.s.name = c('a', 'b')), g_token),
               'Please provide a data frame containing the following columns:\ntrack.s.id, track.s.title, artist.s.name\nSee the function reference for further information.')
})

test_that('Returns a frame with correct additional colnames and content', {
  input <- testTracksArtistsAlbums
  res <- suppressMessages(get_tracks_lyrics_genius(input, g_token))
  res_names <- colnames(res)
  expected_names <- c(colnames(testTracksArtistsAlbums),
                      geniusLyricsVars)
  expect_setequal(res_names, expected_names)

  expect_true(all((res$track.g.quality <= 1 & res$track.g.quality >= 0) | is.na(res$track.g.quality)))
  expect_true(all((res$artist.g.quality <= 1 & res$artist.g.quality >= 0) | is.na(res$artist.g.quality)))
  expect_true(class(res$track.g.id) == 'character')
  expect_true(class(res$track.g.title) == 'character')
  expect_true(class(res$track.g.lyrics) == 'list')
  expect_true(class(res$artist.g.id) == 'character')
  expect_true(class(res$artist.g.name) == 'character')
  expect_true(class(res$artist.g.quality) == 'numeric')

  expect_setequal(res$track.g.lyrics[3][[1]] %>% colnames(), c('line', 'section'))
})
