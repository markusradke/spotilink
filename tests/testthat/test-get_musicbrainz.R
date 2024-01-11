test_that("Data retrieval works", {
  data <-  data.frame(track.s.id = '1MQWtVcs0PKsY4PA6ZvLiy')
  pass <- c("bf4b7a7cffc547d49199cab4ae0b347f","5fe2a814df864abda82b740ecc307661")

  res <- get_spotify(data, pass)
  res <- get_musicbrainz(res)
  dplyr::glimpse(res)
  expect_equal(
    !is.null(dim(res)),
    TRUE)
})
