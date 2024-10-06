test_that('spotify tracks with empty input returns empty frame with updated column names', {
  input <- testTracksArtistsAlbums %>% dplyr::filter(track.s.id == 'thisisnotanid')
  expect_warning(suppressMessages(get_tracks_spotify(input, s_pass)),
                 'get_tracks_spotify was supplied with an empty input.\nReturning empty frame with updated column names.')
  output <- suppressMessages(suppressWarnings(get_tracks_spotify(input)))
  expect_setequal(colnames(output) %>% stringr::str_subset('_old', negate = TRUE), union(colnames(input), spotifyTrackVars))
})

test_that('spotify albums with empty input returns empty frame with updated column names', {
  input <- testTracksArtistsAlbums %>% dplyr::filter(album.s.id == 'thisisnotanid')
  expect_warning(suppressMessages(get_albums_spotify(input, s_pass)),
                 'get_albums_spotify was supplied with an empty input.\nReturning empty frame with updated column names.')
  output <- suppressMessages(suppressWarnings(get_albums_spotify(input)))
  expect_setequal(colnames(output) %>% stringr::str_subset('_old', negate = TRUE), union(colnames(input), spotifyAlbumVars))
})

test_that('spotify artists with empty input returns empty frame with updated column names', {
  input <- testTracksArtistsAlbums %>% dplyr::filter(artist.s.id == 'thisisnotanid')
  expect_warning(suppressMessages(get_artists_spotify(input, s_pass)),
                 'get_artists_spotify was supplied with an empty input.\nReturning empty frame with updated column names.')
  output <- suppressMessages(suppressWarnings(get_artists_spotify(input)))
  expect_setequal(colnames(output) %>% stringr::str_subset('_old', negate = TRUE), union(colnames(input), spotifyArtistVars))
})


test_that('spotify all with empty input returns empty frame with updated column names', {
  input <- testTracksArtistsAlbums %>% dplyr::filter(track.s.id == 'thisisnotanid')
  expect_warning(suppressMessages(get_all_spotify(input, s_pass)),
                 'get_all_spotify was supplied with an empty input.\nReturning empty frame with updated column names.')
  output <- suppressMessages(suppressWarnings(get_all_spotify(input)))
  expect_setequal(colnames(output) %>% stringr::str_subset('_old', negate = TRUE), union(colnames(input), spotifyAllVars))
})
