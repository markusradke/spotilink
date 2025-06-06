test_that('Returns a frame with correct additional colnames and content', {
  input <- testTracksArtistsAlbums %>% dplyr::rename(track.s.firstartist.name = artist.s.name)
  res <- suppressMessages(get_all_deezer(input))
  res_names <- colnames(res)
  expected_names <- c(colnames(input),
                      deezerAllVars)
  expect_setequal(res_names, expected_names)
})

