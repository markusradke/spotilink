prepare_testinput <- function(){
  testTracksArtistsAlbums %>% dplyr::select(-track.s.keyconfidence,
                                            -artist.s.name,
                                            -artist.s.id)
}

spotify_depreceated_vars <- c('track.s.danceability',
                              'track.s.energy',
                              'track.s.key',
                              'track.s.loudness',
                              'track.s.mode',
                              'track.s.speechiness',
                              'track.s.acousticness',
                              'track.s.instrumentalness',
                              'track.s.liveness',
                              'track.s.valence',
                              'track.s.tempo',
                              'track.s.timesignature',
                              'track.s.previewurl')

test_that("spotify track retrieval returns tibble", {
  testinput <- prepare_testinput()
  res <- spotilink::get_tracks_spotify(testinput, s_pass)
  expect_true(tibble::is_tibble(res))
})

test_that("spotify track retrieval without pwd returns correct variables", {
  testinput <- prepare_testinput()
  res <- spotilink::get_tracks_spotify(testinput, s_pass)
  res_cols <- res %>% dplyr::select(-contains('_old')) %>% colnames()
  expected_cols <- spotifyTrackVars[! spotifyTrackVars %in% spotify_depreceated_vars]
  expect_setequal(res_cols, expected_cols)

})

test_that("spotify track retrieval with pwd returns correct variables", {
  testinput <- prepare_testinput()
  res <- spotilink::get_tracks_spotify(testinput, s_pass, pwd = 'spotivey')
  res_cols <- res %>% dplyr::select(-contains('_old')) %>% colnames()
  expected_cols <- spotifyTrackVars
  expect_setequal(res_cols, expected_cols)
})




