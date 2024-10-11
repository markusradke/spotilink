test_that("bugix for steffen", {
  input <- data.frame(artist.s.id = '7t51dSX8ZkKC7VoKRd0lME',
                      artist.s.name = 'Asaf Avidan',
                      track.s.id = '36Nedlx9aSx0lHaorvdqBJ',
                      track.s.title = 'One Day / Reckoning Song (Wankelmut Remix) - Club Mix',
                      track.s.isrc = 'DEQ321200133',
                      track.s.firstartist.name = 'Asaf Avidan')
  resVars <- c(colnames(input), musicbrainzTrackVars, musicbrainzArtistVars)
  res <- suppressMessages(get_tracks_musicbrainz(input) %>% get_artists_musicbrainz())
  file.remove('mb_artists.rds')
  file.remove('mb_tracks.rds')
  mbartist_remove_checkpoints()
  mbtracks_remove_checkpoints()
  expect_setequal(colnames(res), resVars)
})
