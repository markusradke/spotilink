test_that('bugfix musicbrainz albums', {
  if(file.exists('mb_albums_search_1.rds')){file.remove('mb_albums_search_1.rds')}
  if(file.exists('mb_albums_loopup_1.rds')){file.remove('mb_albums_lookup_1.rds')}
  res <- get_albums_musicbrainz(musicbrainzAlbumBugfix)

  expect_equal(length(colnames(res)),
               length(colnames(musicbrainzAlbumBugfix)) + 10)
  if(file.exists('mb_albums.rds')){file.remove('mb_albums.rds')}
})
