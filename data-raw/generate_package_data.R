## code to prepare `DATASET` dataset goes here
musicbrainzWhitelist <- readRDS('./data-raw/whitelist.rds')
source('./data-raw/define_test_data.R')
source('./data-raw/define_variables.R')

s_pass <- c("bf4b7a7cffc547d49199cab4ae0b347f","5fe2a814df864abda82b740ecc307661")
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
         correctTypesAll,
         spotifyAudioanalysisVars,
         correctTypesAudioanalysis,
         largeArtistTest,
         s_pass,
         dc_pass,
         g_token,
         internal = TRUE,
         overwrite = TRUE)
