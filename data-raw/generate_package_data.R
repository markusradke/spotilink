## code to prepare `DATASET` dataset goes here
rm(list=ls())
musicbrainzWhitelist <- readRDS('./data-raw/whitelist.rds')
source('./data-raw/define_test_data.R')
source('./data-raw/define_variables.R')

s_pass <-  c('eb2a169a0bc84c3fa675cf52e59a0adf', '93b3486b9cea4af8b68231af3114501a')
spotivey_pass <- c('6f069a93062b4333bedd796f9312904c','ddcee099adcd4147a590beeb4bae4475')
dc_pass <- c('bSyzKCYCBBwMYsMfxUhj', 'lZvwpvEOdZnrJsWewPxjvYUgdeWDCENu')
g_token <- '-jEcwl0b2ANL3rmxjbL5gcsHcmtkV3-sw55SslkLtWBFn3tpBrdHS0ANZteuQjco'
use_data(musicbrainzWhitelist, # external references

         testTracksArtistsAlbums, # test data
         testTracksArtistsAlbums_larger,
         largeArtistTest,
         testAcousticbrainz,
         testresults,
         musicbrainzAlbumBugfix,

         spotifyTrackVars,# platform var sets
         musicbrainzTrackVars,
         spotifyAlbumVars,
         musicbrainzAlbumVars,
         spotifyArtistVars,
         musicbrainzArtistVars,
         discogsAlbumVars,
         geniusLyricsVars,
         deezerTrackVars,
         deezerAlbumVars,
         deezerArtistVars,
         deezerAllVars,
         acousticbrainzTrackVarsHighlevel,
         acousticbrainzTrackVarsLowlevel,
         allVars,

         spotifyAllVars, # aggregated var sets
         musicbrainzAllVars,
         acousticbrainzTrackVars,

         spotifyAudioanalysisVars, # special vars
         correctTypesAll,
         correctTypesAudioanalysis,

         trackIdentifier, # feature var sets
         artistIdentifier,
         releaseIdentifier,
         linkageVariables,
         labelIdentifier,
         artistDescriptors,
         releaseDescriptors,
         platformHighlevelAudioDescriptors,
         audioTrackMetadata,
         mlHighlevelAudioClassificators,
         mlHighlevelAudioDescriptors,
         aggregatedTrackLowlevelFeatures,
         genreDescriptors,
         mlGenreClassificators,
         genreTags,
         platformPopularity,
         coverLyricsExplicitness,
         allFeatureSets,


         s_pass, # passports
         spotivey_pass,
         dc_pass,
         g_token,
         internal = TRUE,
         overwrite = TRUE)
