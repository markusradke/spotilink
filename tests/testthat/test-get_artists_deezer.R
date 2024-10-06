test_that('Assertion all variables in input data frame', {
  expect_error(suppressWarnings(get_artists_deezer(data.frame(artist.s.id = c('a', 'b')))),
               'Please provide a data frame containing the following columns:\nartist.s.id, artist.s.name\nSee the function reference for further information.')
  expect_error(suppressWarnings(get_artists_deezer(data.frame(artist.s.name = c('a', 'b')))),
               'Please provide a data frame containing the following columns:\nartist.s.id, artist.s.name\nSee the function reference for further information.')

})

test_that('Returns a frame with correct additional colnames and content', {
  input <- testTracksArtistsAlbums
  res <- suppressWarnings(suppressMessages(get_artists_deezer(input)))
  res_names <- colnames(res)
  expected_names <- c(colnames(input),
                      deezerArtistVars)
  expect_setequal(res_names, expected_names)

  expect_true(all((res$artist.dz.quality <= 1 & res$artist.dz.quality >= 0) | is.na(res$artist.dz.quality)))
  expect_true(class(res$artist.dz.id) == 'character')
  expect_true(class(res$artist.dz.name) == 'character')
  expect_true(class(res$artist.dz.follower) == 'integer')
  expect_true(class(res$artist.dz.nalbums) == 'integer')
  expect_true(class(res$artist.dz.quality) == 'numeric')
  expect_true(class(res$artist.dz.toptracks) == 'list')
})
