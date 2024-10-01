test_that('Assertion all variables in input data frame', {
  expect_error(get_tracks_deezer(data.frame(track.s.id = c('a', 'b'), track.s.title = c('a', 'b'))),
               'Please provide a data frame containing the following columns:\ntrack.s.id, track.s.title, track.s.firstartist.name, album.s.title\nSee the function reference for further information.')
  expect_error(get_tracks_deezer(data.frame(track.s.id = c('a', 'b'), track.s.firstartist.name = c('a', 'b'))),
               'Please provide a data frame containing the following columns:\ntrack.s.id, track.s.title, track.s.firstartist.name, album.s.title\nSee the function reference for further information.')
  expect_error(get_tracks_deezer(data.frame(track.s.title = c('a', 'b'), track.s.firstartist.name = c('a', 'b'))),
               'Please provide a data frame containing the following columns:\ntrack.s.id, track.s.title, track.s.firstartist.name, album.s.title\nSee the function reference for further information.')
})

test_that('Returns a frame with correct additional colnames and content', {
  input <- testTracksArtistsAlbums %>% dplyr::rename(track.s.firstartist.name = artist.s.name)
  res <- suppressMessages(get_tracks_deezer(input))
  res_names <- colnames(res)
  expected_names <- c(colnames(input),
                      deezerTrackVars)
  expect_setequal(res_names, expected_names)

  expect_true(all((res$track.dz.quality <= 1 & res$track.dz.quality >= 0) | is.na(res$track.dz.quality)))
  expect_true(all((res$artist.dz.quality <= 1 & res$artist.dz.quality >= 0) | is.na(res$artist.dz.quality)))
  expect_true(all((res$album.dz.quality <= 1 & res$album.dz.quality >= 0) | is.na(res$album.dz.quality)))
  expect_true(class(res$track.dz.id) == 'character')
  expect_true(class(res$track.dz.title) == 'character')
  expect_true(class(res$track.dz.isrc) == 'character')
  expect_true(class(res$track.dz.duration) == 'integer')
  expect_true(class(res$track.dz.rank) == 'integer')
  expect_true(class(res$track.dz.explicit) == 'logical')
  expect_true(class(res$track.dz.explicitinfo) == 'character')
  expect_true(class(res$track.dz.tempo) == 'integer')
  expect_true(class(res$track.dz.loudness) == 'numeric')
  expect_true(class(res$artist.dz.id) == 'character')
  expect_true(class(res$artist.dz.name) == 'character')
  expect_true(class(res$artist.dz.quality) == 'numeric')
})
