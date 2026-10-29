# Synthetic fixtures and private helper environments keep these tests offline.
audio_features_fixture <- function() {
  data.frame(id = 'test-track', danceability = .6, energy = .8, key = 1,
             loudness = -5, mode = 1, speechiness = .02, acousticness = .1,
             instrumentalness = .3, liveness = .2, valence = .7, tempo = 124,
             time_signature = 4)
}

test_that('complete audio-feature responses retain their values and types', {
  raw <- audio_features_fixture()
  expected <- raw
  expected$key <- 'C#'
  expected$mode <- 'major'
  expected$time_signature <- '4/4'
  names(expected) <- c('track.s.id', 'track.s.danceability', 'track.s.energy',
                      'track.s.key', 'track.s.loudness', 'track.s.mode',
                      'track.s.speechiness', 'track.s.acousticness',
                      'track.s.instrumentalness', 'track.s.liveness',
                      'track.s.valence', 'track.s.tempo', 'track.s.timesignature')
  expect_identical(clean_features(raw), expected)
})

test_that('missing time signature preserves all remaining audio features', {
  raw <- audio_features_fixture()
  complete <- clean_features(raw)
  raw$time_signature <- NULL
  result <- clean_features(raw)
  expect_true(is.na(result$track.s.timesignature))
  other <- setdiff(names(complete), 'track.s.timesignature')
  expect_identical(result[other], complete[other])
})

test_that('other optional fields can be absent without losing track IDs', {
  raw <- audio_features_fixture()
  raw$key <- raw$valence <- NULL
  result <- clean_features(raw)
  expect_true(is.na(result$track.s.key))
  expect_true(is.na(result$track.s.valence))
  expect_identical(result$track.s.id, 'test-track')
  expect_identical(result$track.s.tempo, 124)
})

test_that('empty audio features have a typed schema, invalid rows still fail', {
  for (raw in list(data.frame(), data.frame(value = NA))) {
    result <- clean_features(raw)
    expect_equal(nrow(result), 0L)
    expect_identical(result$track.s.id, character())
    expect_identical(result$track.s.tempo, numeric())
    expect_identical(result$track.s.timesignature, character())
  }
  raw <- audio_features_fixture()
  raw$id <- NULL
  expect_error(clean_features(raw), 'rows without track IDs', fixed = TRUE)
})

test_that('empty audio features preserve inputs and allow track retrieval', {
  # Replace request/checkpoint helpers only in a private function environment;
  # the installed package namespace and real API functions remain untouched.
  env <- new.env(parent = environment(get_from_API))
  env$get_from_API <- get_from_API
  environment(env$get_from_API) <- env
  env$read_checkpoint <- function(...) {
    list(last_index = 0L, saved_data = NULL, last_checkpoint = '')
  }
  env$remove_checkpoints_spotify <- function(...) invisible(NULL)
  env$get_from_IDs <- function(ids, pullFunction, ...) env$response
  input <- data.frame(track.s.id = c('test-track', 'other-track'),
                      input_key = c('keep-first', 'keep-second'))
  for (raw in list(data.frame(), data.frame(value = NA))) {
    env$response <- raw
    features <- env$get_from_API(input, 'track.s.id',
                                spotifyr::get_track_audio_features,
                                clean_features, 50)
    expect_identical(features[names(input)], input)
    expect_true(all(is.na(features$track.s.tempo)))
    expect_true(all(is.na(features$track.s.timesignature)))

    # A following request still receives both IDs and can add track metadata.
    env$response <- data.frame(track.s.id = input$track.s.id,
                               track.s.title = c('First title', 'Second title'))
    tracks <- env$get_from_API(features, 'track.s.id', spotifyr::get_tracks,
                              identity, 50)
    expect_equal(nrow(tracks), 2L)
    expect_identical(tracks$track.s.title, c('First title', 'Second title'))
    expect_identical(tracks$input_key, input$input_key)
  }
})

test_that('empty responses from other endpoints retain existing behavior', {
  env <- new.env(parent = environment(get_from_API))
  env$get_from_API <- get_from_API
  environment(env$get_from_API) <- env
  env$read_checkpoint <- function(...) {
    list(last_index = 0L, saved_data = NULL, last_checkpoint = '')
  }
  env$remove_checkpoints_spotify <- function(...) invisible(NULL)
  env$get_from_IDs <- function(...) data.frame(track.s.id = character())
  result <- env$get_from_API(data.frame(track.s.id = 'test-track'),
                            'track.s.id', spotifyr::get_tracks, identity, 50)
  expect_equal(nrow(result), 0L)
})
