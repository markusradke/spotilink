test_that('all features false returns only track.s.id', {
  res <- select_features(testresults)
  expect_setequal(colnames(res), c('track.s.id'))
})

test_that('all features to select are in variables set',{
  expect_equal(allFeatureSets[! allFeatureSets %in% allVars], character())
})

test_that('all features in variable set can be selected',{
  expect_equal(allVars[! allVars %in% allFeatureSets], character())
})
