## code to prepare `DATASET` dataset goes here
musicbrainzWhitelist <- readRDS('./data-raw/whitelist.rds')
source('./data-raw/define_test_data.R')
source('./data-raw/define_variables.R')

s_pass <- c("528da7272bb442c0a5953d705734df61","9ff41aa7645c4f65851a4ccc81014a0b")
dc_pass <- c('bSyzKCYCBBwMYsMfxUhj', 'lZvwpvEOdZnrJsWewPxjvYUgdeWDCENu')
g_token <- '-jEcwl0b2ANL3rmxjbL5gcsHcmtkV3-sw55SslkLtWBFn3tpBrdHS0ANZteuQjco'
use_data(musicbrainzWhitelist,
         testTracksArtistsAlbums,
         testTracksArtistsAlbums_larger,
         spotifyTrackVars,
         musicbrainzTrackVars,
         spotifyAlbumVars,
         musicbrainzAlbumVars,
         spotifyArtistVars,
         musicbrainzArtistVars,
         spotifyAllVars,
         musicbrainzAllVars,
         discogsAlbumVars,
         geniusLyricsVars,
         deezerTrackVars,
         deezerAlbumVars,
         deezerArtistVars,
         deezerAllVars,
         acousticbrainzTrackVars,
         acousticbrainzTrackVarsHighlevel,
         acousticbrainzTrackVarsLowlevel,
         correctTypesAll,
         spotifyAudioanalysisVars,
         correctTypesAudioanalysis,
         largeArtistTest,
         testAcousticbrainz,
         testresults,
         s_pass,
         dc_pass,
         g_token,
         internal = TRUE,
         overwrite = TRUE)
