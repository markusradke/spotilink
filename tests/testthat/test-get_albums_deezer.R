test_that('Assertion all variables in input data frame', {
  expect_error(get_albums_deezer(data.frame(album.s.id = c('a', 'b'), album.s.title = c('a', 'b'))),
               'Please provide a data frame containing the following columns:\nalbum.s.id, album.s.title, track.s.firstartist.name\nSee the function reference for further information.')
  expect_error(get_albums_deezer(data.frame(track.s.id = c('a', 'b'), track.s.firstartist.name = c('a', 'b'))),
               'Please provide a data frame containing the following columns:\nalbum.s.id, album.s.title, track.s.firstartist.name\nSee the function reference for further information.')
  expect_error(get_albums_deezer(data.frame(track.s.title = c('a', 'b'), track.s.firstartist.name = c('a', 'b'))),
               'Please provide a data frame containing the following columns:\nalbum.s.id, album.s.title, track.s.firstartist.name\nSee the function reference for further information.')
})

test_that('Returns a frame with correct additional colnames and content', {
  input <- testTracksArtistsAlbums %>% dplyr::rename(track.s.firstartist.name = artist.s.name)
  res <- suppressMessages(get_albums_deezer(input))
  res_names <- colnames(res)
  expected_names <- c(colnames(input),
                      deezerAlbumVars)
  expect_setequal(res_names, expected_names)

  expect_true(all((res$album.dz.firstartist.quality <= 1 & res$album.dz.firstartist.quality >= 0) | is.na(res$album.dz.firstartist.quality)))
  expect_true(all((res$album.dz.quality <= 1 & res$album.dz.quality >= 0) | is.na(res$album.dz.quality)))
  expect_true(class(res$album.dz.id) == 'character')
  expect_true(class(res$album.dz.title) == 'character')
  expect_true(class(res$album.dz.upc) == 'character')
  expect_true(class(res$album.dz.duration) == 'integer')
  expect_true(class(res$album.dz.totaltracks) == 'integer')
  expect_true(class(res$album.dz.follower) == 'integer')
  expect_true(class(res$album.dz.releasedate) == 'Date')
  expect_true(class(res$album.dz.type) == 'character')
  expect_true(class(res$album.dz.explicitlyrics) == 'logical')
  expect_true(class(res$album.dz.explicitlyricsinfo) == 'character')
  expect_true(class(res$album.dz.explicitcoverinfo) == 'character')
  expect_true(class(res$album.dz.label) == 'character')
  expect_true(class(res$album.dz.firstgenre.id) == 'integer')
  expect_true(class(res$album.dz.firstgenre.name) == 'character')
  expect_true(class(res$album.dz.genres) == 'list')
  expect_true(class(res$album.dz.firstartist.id) == 'character')
  expect_true(class(res$album.dz.firstartist.name) == 'character')
  expect_true(class(res$album.dz.firstartist.quality) == 'numeric')
})
