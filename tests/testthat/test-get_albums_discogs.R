test_that('Assertion all variables in input data frame', {
  expect_error(get_albums_discogs(data.frame(album.s.id = c('a', 'b'), album.s.title = c('a', 'b')), dc_pass),
               'Please provide a data frame containing the following columns:\nalbum.s.id, album.s.title, album.s.firstartist.name\nSee the function reference for further information.')
  expect_error(get_albums_discogs(data.frame(album.s.id = c('a', 'b'), artist.s.name = c('a', 'b')), dc_pass),
               'Please provide a data frame containing the following columns:\nalbum.s.id, album.s.title, album.s.firstartist.name\nSee the function reference for further information.')
  expect_error(get_albums_discogs(data.frame(album.s.title = c('a', 'b'), artist.s.name = c('a', 'b')), dc_pass),
               'Please provide a data frame containing the following columns:\nalbum.s.id, album.s.title, album.s.firstartist.name\nSee the function reference for further information.')
})

test_that('Returns a frame with correct additional colnames and content', {
  dc_pass <- c('bSyzKCYCBBwMYsMfxUhj', 'lZvwpvEOdZnrJsWewPxjvYUgdeWDCENu')
  input <- testTracksArtistsAlbums %>% dplyr::rename(album.s.firstartist.name = artist.s.name)
  res <- suppressMessages(get_albums_discogs(input, dc_pass))
  res_names <- colnames(res)
  expected_names <- c(colnames(input),
                     discogsAlbumVars)
  expect_setequal(res_names, expected_names)

  expect_true(all((res$album.dc.quality <= 1 & res$album.dc.quality >= 0) | is.na(res$album.dc.quality)))
  expect_true(all((res$artist.dc.quality <= 1 & res$artist.dc.quality >= 0) | is.na(res$album.dc.quality)))
  expect_true(class(res$album.dc.id) == 'character')
  expect_true(class(res$album.dc.title) == 'character')
  expect_true(class(res$album.dc.genres) == 'list')
  expect_true(class(res$album.dc.styles) == 'list')
  expect_true(class(res$album.dc.quality) == 'numeric')
  expect_true(class(res$artist.dc.name) == 'character')
  expect_true(class(res$artist.dc.quality) == 'numeric')

  expect_true(all(res$album.dc.firstgenre %in% c('Blues',
                                            'Brass & Military',
                                            'Children\'s',
                                            'Classical',
                                            'Electronic',
                                            'Folk, World, & Country',
                                            'Funk / Soul',
                                            'Hip-Hop',
                                            'Jazz',
                                            'Latin',
                                            'Non-Music',
                                            'Pop',
                                            'Reggae',
                                            'Rock',
                                            'Stage & Screen', NA)))
})

test_that('Renames already existing columns', {
  dc_pass <- c('bSyzKCYCBBwMYsMfxUhj', 'lZvwpvEOdZnrJsWewPxjvYUgdeWDCENu')

  input <- testTracksArtistsAlbums %>% dplyr::rename(album.s.firstartist.name = artist.s.name)
  input$album.dc.genres <- NA

  res <- suppressMessages(get_albums_discogs(input, dc_pass))
  res_names <- colnames(res)
  expected_names <- c(colnames(input),
                      discogsAlbumVars,
                      'album.dc.genres_old')
  expect_setequal(res_names, expected_names)
})
