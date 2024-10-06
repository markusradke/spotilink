## code to prepare `DATASET` dataset goes here
musicbrainzWhitelist <- readRDS('./data-raw/whitelist.rds')
source('./data-raw/define_test_data.R')
source('./data-raw/define_variables.R')

s_pass <- c("eb2a169a0bc84c3fa675cf52e59a0adf","93b3486b9cea4af8b68231af3114501a")
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
         correctTypesAll,
         spotifyAudioanalysisVars,
         correctTypesAudioanalysis,
         largeArtistTest,
         testAcousticbrainz,
         s_pass,
         dc_pass,
         g_token,
         internal = TRUE,
         overwrite = TRUE)
