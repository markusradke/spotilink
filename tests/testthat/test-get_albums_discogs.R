test_that('Assertion all variables in input data frame', {
  dc_passport <- c('bSyzKCYCBBwMYsMfxUhj', 'lZvwpvEOdZnrJsWewPxjvYUgdeWDCENu')

  expect_error(get_albums_discogs(data.frame(album.s.id = c('a', 'b'), album.s.title = c('a', 'b')), dc_passport),
               'Please provide a data frame containing the following columns:\nalbum.s.id, album.s.title, artist.s.name\nSee the function reference for further information.')
  expect_error(get_albums_discogs(data.frame(album.s.id = c('a', 'b'), artist.s.name = c('a', 'b')), dc_passport),
               'Please provide a data frame containing the following columns:\nalbum.s.id, album.s.title, artist.s.name\nSee the function reference for further information.')
  expect_error(get_albums_discogs(data.frame(album.s.title = c('a', 'b'), artist.s.name = c('a', 'b')), dc_passport),
               'Please provide a data frame containing the following columns:\nalbum.s.id, album.s.title, artist.s.name\nSee the function reference for further information.')
})

test_that('Returns a frame with correct additional colnames and content', {
  dc_passport <- c('bSyzKCYCBBwMYsMfxUhj', 'lZvwpvEOdZnrJsWewPxjvYUgdeWDCENu')
  input <- testTracksArtistsAlbums
  res <- get_albums_discogs(input, dc_passport)
  res_names <- colnames(res)
  expected_names <- c(colnames(testTracksArtistsAlbums),
                      'album.dc.id', 'album.dc.name', 'album.dc.genres', 'album.dc.style',
                      'album.dc.firstgenre', 'album.dc.firststyle',
                      'album.dc.quality', 'artist.dc.name', 'artist.dc.quality')
  expect_setequal(res_names, expected_names)

  expect_true(all(res$album.dc.quality <= 1 & res$album.dc.quality >= 0))
  expect_true(all(res$artist.dc.quality <= 1 & res$artist.dc.quality >= 0))
  expect_true(class(res$album.dc.id) == 'character')
  expect_true(class(res$album.dc.name) == 'character')
  expect_true(class(res$album.dc.genre) == 'character')
  expect_true(class(res$album.dc.stlye) == 'character')
  expect_true(class(res$album.dc.quality) == 'numeric')
  expect_true(class(res$artist.dc.name) == 'character')
  expect_true(class(res$artist.dc.quality) == 'numeric')


  expect_true(all(res$album.dc.genre %in% c('Blues',
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
                                            'Stage & Screen')))
})

test_that('Renames already existing columns', {
  dc_passport <- c('bSyzKCYCBBwMYsMfxUhj', 'lZvwpvEOdZnrJsWewPxjvYUgdeWDCENu')

  input <- testTracksArtistsAlbums
  input$album.dc.genres <- NA

  res <- get_albums_discogs(input, dc_passport)
  res_names <- colnames(res)
  expected_names <- c(colnames(testTracksArtistsAlbums),
                      'album.dc.id', 'album.dc.name', 'album.dc.genres', 'album.dc.style',
                      'album.dc.firstgenre', 'album.dc.firststyle',
                      'album.dc.quality', 'artist.dc.name', 'artist.dc.quality',
                      'album.dc.genres_old')
  expect_setequal(res_names, expected_names)
})
