testTracksArtistsAlbums <- data.frame(
 track.s.title = c('Der Geist hilft unser Schwachheit auf, BWV 226',
                   'Hiding My',
                   'teenage dream'),
 track.s.id = c('4ZXLWTmQFzM02hZwMiZfgS',
                '4109koVh6n3SrERQShalwB',
                '7fesNTgTEMEH0ye8MOgEDY'),
 artist.s.name = c('Johann Sebastian Bach',
                   'NMND',
                   'Olivia Rodrigo'),
 artist.s.id = c('5aIqB5nVVvmFsvSdExz408',
                 '5MZbjW7nf4WZYfnAwDfGtx',
                 '1McMsnEElThX1knmY4oliG'),
 album.s.id = c('2rlWvQ1GuTOSkyNpmcoaMC',
                '2ird30RHEXojdZfL7NBSOd',
                '1xJHno7SmdVtZAtXbdbDZp'),
 album.s.title = c('Bach: Motets',
                  'Hiding My',
                  'GUTS'),
 track.s.keyconfidence = c(0., 0., 0.))


largeArtistTest <- readRDS('./data-raw/largeArtistTest.rds')
testTracksArtistsAlbums_larger <- readRDS('./data-raw/testTracksArtistsAlbums_larger.rds')
testAcousticbrainz <- readRDS('./data-raw/testAcousticbrainzData.rds')
testresults <- readRDS('./data-raw/testresults.RDS')
musicbrainzAlbumBugfix <- readRDS('./data-raw/musicbrainz_album_fix_testdata.rds')
genius_translation_test <- readRDS('data-raw/genius_translation_test.rds')
