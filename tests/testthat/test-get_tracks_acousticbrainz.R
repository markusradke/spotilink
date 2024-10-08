test_that('Assertion all variables in input data frame', {
  expect_error(get_tracks_acousticbrainz(data.frame(track.mb.id = c('a', 'b'))),
               'Please provide a data frame containing the following columns:\ntrack.mb.id, track.mb.quality\nSee the function reference for further information.')
  expect_error(get_tracks_acousticbrainz(data.frame(track.mb.quality = c(0,1))),
               'Please provide a data frame containing the following columns:\ntrack.mb.id, track.mb.quality\nSee the function reference for further information.')
})


test_that('Returns a frame with correct additional colnames and content', {
  input <- testAcousticbrainz
  res <- suppressMessages(get_tracks_acousticbrainz(input))
  res_names <- colnames(res)
  expected_names <- c(colnames(input),
                      acousticbrainzTrackVars)
  expect_setequal(res_names, expected_names)
})

test_that('Handles NA', {
  input <- get_spotify_ids("Vini Vici", tracks="The Tribe", pass = s_pass)
  res <- suppressWarnings(suppressMessages(input %>%
    get_tracks_spotify(s_pass) %>%
    get_tracks_musicbrainz() %>%
    get_tracks_acousticbrainz()))
  res_names <- colnames(res)
  expected_names <- c(colnames(input),
                      'track.s.title_old',
                      spotifyTrackVars,
                      musicbrainzTrackVars,
                      acousticbrainzTrackVars)
  expect_setequal(res_names, expected_names)
})
