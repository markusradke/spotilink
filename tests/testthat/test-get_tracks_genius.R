test_that('Assertion all variables in input data frame', {
  expect_error(get_tracks_genius(data.frame(track.s.id = c('a', 'b'), track.s.title = c('a', 'b')), g_token),
               'Please provide a data frame containing the following columns:\ntrack.s.id, track.s.title, track.s.firstartist.name\nSee the function reference for further information.')
  expect_error(get_tracks_genius(data.frame(track.s.id = c('a', 'b'), track.s.firstartist.name = c('a', 'b')), g_token),
               'Please provide a data frame containing the following columns:\ntrack.s.id, track.s.title, track.s.firstartist.name\nSee the function reference for further information.')
  expect_error(get_tracks_genius(data.frame(track.s.title = c('a', 'b'), track.s.firstartist.name = c('a', 'b')), g_token),
               'Please provide a data frame containing the following columns:\ntrack.s.id, track.s.title, track.s.firstartist.name\nSee the function reference for further information.')
})

test_that('Returns a frame with correct additional colnames and content', {
  input <- testTracksArtistsAlbums %>% dplyr::rename('track.s.firstartist.name' = 'artist.s.name')
  res <- suppressMessages(get_tracks_genius(input, g_token))
  res_names <- colnames(res)
  expected_names <- c(colnames(input),
                      geniusLyricsVars)
  expect_setequal(res_names, expected_names)

  expect_true(all((res$track.g.quality <= 1 & res$track.g.quality >= 0) | is.na(res$track.g.quality)))
  expect_true(all((res$artist.g.quality <= 1 & res$artist.g.quality >= 0) | is.na(res$artist.g.quality)))
  expect_true(class(res$track.g.id) == 'character')
  expect_true(class(res$track.g.title) == 'character')
  expect_true(class(res$track.g.lyrics) == 'list')
  expect_true(class(res$track.g.firstartist.id) == 'character')
  expect_true(class(res$track.g.firstartist.name) == 'character')
  expect_true(class(res$track.g.firstartist.quality) == 'numeric')
  expect_true(class(res$track.g.lyricsstate) == 'character')

  expect_setequal(res$track.g.lyrics[3][[1]] %>% colnames(), c('line', 'section'))
})
