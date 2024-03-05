## code to prepare `DATASET` dataset goes here
musicbrainzWhitelist <- readRDS('./data-raw/whitelist.rds')
source('./data-raw/define_test_data.R')
source('./data-raw/define_variables.R')

use_data(musicbrainzWhitelist,
         testTracksArtistsAlbums,
         spotifyTrackVars,
         musicbrainzTrackVars,
         spotifyAlbumVars,
         musicbrainzAlbumVars,
         spotifyArtistVars,
         musicbrainzArtistVars,
         spotifyAllVars,
         musicbrainzAllVars,
         correctTypesAll,
         spotifyAudioanalysisVars,
         correctTypesAudioanalysis,
         internal = TRUE,
         overwrite = TRUE)
