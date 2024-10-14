test_that("get_multiple_returns the right cols", {
  res <- suppressMessages(get_multiple_recommendation_for_genre_seed_spotify('rock', s_pass, n_iterations = 1))
  expect_setequal(colnames(res$recommendations), spotifyTrackVars)
  expect_equal(nrow(res$recommendations),  100)
})

test_that('Anime also works', {
  res <- get_multiple_recommendation_for_genre_seed_spotify('anime', s_pass)
  expect_setequal(colnames(res$recommendations), spotifyTrackVars, n_iterations = 1)
})
