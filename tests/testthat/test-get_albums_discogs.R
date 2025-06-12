test_that('Assertion all variables in input data frame', {
  expect_error(get_albums_discogs(data.frame(album.s.id = c('a', 'b'), album.s.title = c('a', 'b')), dc_pass),
               'Please provide a data frame containing the following columns:\nalbum.s.id, album.s.title, album.s.firstartist.name, album.s.releaseyear\nSee the function reference for further information.')
  expect_error(get_albums_discogs(data.frame(album.s.id = c('a', 'b'), artist.s.name = c('a', 'b')), dc_pass),
               'Please provide a data frame containing the following columns:\nalbum.s.id, album.s.title, album.s.firstartist.name, album.s.releaseyear\nSee the function reference for further information.')
  expect_error(get_albums_discogs(data.frame(album.s.title = c('a', 'b'), artist.s.name = c('a', 'b')), dc_pass),
               'Please provide a data frame containing the following columns:\nalbum.s.id, album.s.title, album.s.firstartist.name, album.s.releaseyear\nSee the function reference for further information.')
})

test_that('Returns a frame with correct additional colnames and content', {
  dc_pass <- c('bSyzKCYCBBwMYsMfxUhj', 'lZvwpvEOdZnrJsWewPxjvYUgdeWDCENu')
  input <-  suppressMessages(testTracksArtistsAlbums %>% get_albums_spotify(s_pass))
  res <- suppressMessages(get_albums_discogs(input, dc_pass))
  res_names <- colnames(res)
  expected_names <- c(colnames(input),
                     discogsAlbumVars)
  expect_setequal(res_names, expected_names)

  expect_true(all((res$album.dc.quality <= 1 & res$album.dc.quality >= 0) | is.na(res$album.dc.quality)))
  expect_true(all((res$album.dc.firstartist.quality <= 1 & res$album.dc.firstartist.quality >= 0) | is.na(res$album.dc.firstartist.quality)))
  expect_true(class(res$album.dc.id) == 'character')
  expect_true(class(res$album.dc.title) == 'character')
  expect_true(class(res$album.dc.genres) == 'list')
  expect_true(class(res$album.dc.styles) == 'list')
  expect_true(class(res$album.dc.quality) == 'numeric')
  expect_true(class(res$album.dc.firstartist.name) == 'character')
  expect_true(class(res$album.dc.firstartist.quality) == 'numeric')
  expect_true(class(res$album.dc.country) == 'character')

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

  input <- suppressMessages(testTracksArtistsAlbums %>% get_albums_spotify(s_pass))
  input$album.dc.genres <- NA

  res <- suppressMessages(get_albums_discogs(input, dc_pass))
  res_names <- colnames(res)
  expected_names <- c(colnames(input),
                      discogsAlbumVars,
                      'album.dc.genres_old')
  expect_setequal(res_names, expected_names)
})

test_that('release year bug fix for topresults', {
  input <- data.frame(album.s.id = '2UgmfoJF7x7cQmWADnoQdG',
                      album.s.title = 'Reload (Vocal Version / Radio Edit)',
                      album.s.firstartist.name = 'Sebastian Ingrosso',
                      album.s.releaseyear = 2013L)
  res <- suppressMessages(get_albums_discogs(input, dc_pass))
  res_names <- colnames(res)
  expected_names <- c(colnames(input),
                      discogsAlbumVars)
  expect_setequal(res_names, expected_names)

})
