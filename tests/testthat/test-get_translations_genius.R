test_that("df returns the right content", {
  res <- get_translations_genius(genius_translation_test, g_token, language = 'en')
  res_colnames <- colnames(res)
  expect_true(c('track.g.language' %in% res_colnames))
  expect_true(c('track.g.translations' %in% res_colnames))
  expect_true(c('track.g.lyrics_en' %in% res_colnames))

  expect_equal(res[3,]$track.g.lyrics, res[3,]$track.g.lyrics_en)
  expect_equal(nrow(res[1,]$track.g.lyrics_en[[1]]), 24) # actually gets translation
  expect_equal(ncol(res[1,]$track.g.lyrics_en[[1]]), 2) # actually gets translation
})


test_that("get available languages works", {
  translations_list <- list(
    list(testid = 1, language = "de",  url = "testurl1"),
    list(testid = 2, language = "en",  url = "testurl2")
  )


  test <- tibble::tibble(
    track.g.id = "1519307",
    track.g.language = "fr",
    track.g.translations = list(translations_list),
    lyrics_translation = NA
  )

  testresult <- tibble::tibble(
    available_language = c("de", "en"),
    url = c(
      "testurl1",
      "testurl2"
    )
  )

  expect_equal(get_available_languages(test), testresult)
})
