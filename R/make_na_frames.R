make_na_frame_spotify_tracks <- function(track.s.id){
  data.frame(setNames(replicate(length(spotifyTrackVars), NA, simplify = FALSE), spotifyTrackVars)) %>%
    dplyr::mutate(track.s.id = track.s.id)
}

make_na_frame_spotify_albums <- function(album.s.id){
  data.frame(setNames(replicate(length(spotifyAlbumVars), NA, simplify = FALSE), spotifyAlbumVars)) %>%
    dplyr::mutate(album.s.id = album.s.id)
}

make_na_frame_spotify_artists <- function(artist.s.id){
  data.frame(setNames(replicate(length(spotifyArtistVars), NA, simplify = FALSE), spotifyArtistVars)) %>%
    dplyr::mutate(artist.s.id = artist.s.id)
}

make_na_frame_spotify_all <- function(track.s.id){
  data.frame(setNames(replicate(length(spotifyAllVars), NA, simplify = FALSE), spotifyAllVars)) %>%
    dplyr::mutate(track.s.id = track.s.id)
}

make_na_frame_musicbrainz_tracks <- function(track.s.id){
  data.frame(setNames(replicate(length(musicbrainzTrackVars), NA, simplify = FALSE), musicbrainzTrackVars)) %>%
    dplyr::mutate(track.s.id = track.s.id)
}

make_na_frame_musicbrainz_albums <- function(album.s.id){
  data.frame(setNames(replicate(length(musicbrainzAlbumVars), NA, simplify = FALSE), musicbrainzAlbumVars)) %>%
    dplyr::mutate(album.s.id = album.s.id)
}

make_na_frame_musicbrainz_artists <- function(artist.s.id){
  data.frame(setNames(replicate(length(musicbrainzArtistVars), NA, simplify = FALSE), musicbrainzArtistVars)) %>%
    dplyr::mutate(artist.s.id = artist.s.id)
}

make_na_frame_musicbrainz_albums <- function(track.s.id){
  data.frame(setNames(replicate(length(musicbrainzAllVars), NA, simplify = FALSE), musicbrainzAllVars)) %>%
    dplyr::mutate(track.s.id = track.s.id)
}

make_na_frame_deezer_tracks <- function(track.s.id){
  data.frame(setNames(replicate(length(deezerTrackVars), NA, simplify = FALSE), deezerTrackVars)) %>%
    dplyr::mutate(track.s.id = track.s.id)
}

make_na_frame_deezer_albums <- function(album.s.id){
  data.frame(setNames(replicate(length(deezerAlbumVars), NA, simplify = FALSE), deezerAlbumVars)) %>%
    dplyr::mutate(album.s.id = album.s.id)
}

make_na_frame_deezer_artists <- function(artist.s.id){
  data.frame(setNames(replicate(length(deezerArtistVars), NA, simplify = FALSE), deezerArtistVars)) %>%
    dplyr::mutate(artist.s.id = artist.s.id)
}

make_na_frame_genius_tracks <- function(track.s.id){
  data.frame(setNames(replicate(length(geniusLyricsVars), NA, simplify = FALSE), geniusLyricsVars)) %>%
    dplyr::mutate(track.s.id = track.s.id)
}

make_na_frame_acousticbrainz_tracks <- function(track.s.id){
  data.frame(setNames(replicate(length(acousticbrainzTrackVars), NA, simplify = FALSE), acousticbrainzTrackVars)) %>%
    dplyr::mutate(track.s.id = track.s.id)
}

make_na_frame_audioanalysis_spotify <- function(track.s.id){
  data.frame(setNames(replicate(length(spotifyAudioanalysisVars), NA, simplify = FALSE), spotifyAudioanalysisVars)) %>%
    dplyr::mutate(track.s.id = track.s.id)
}

make_na_frame_acousticbrainz_tracks_highlevel <- function(track.mb.id){
  data.frame(setNames(replicate(length(acousticbrainzTrackVarsHighlevel), NA, simplify = FALSE), acousticbrainzTrackVarsHighlevel)) %>%
    dplyr::mutate(track.mb.id = track.mb.id, track.ab.id = track.mb.id)
}

make_na_frame_acousticbrainz_tracks_lowlevel <- function(track.mb.id){
  data.frame(setNames(replicate(length(acousticbrainzTrackVarsLowlevel), NA, simplify = FALSE), acousticbrainzTrackVarsLowlevel)) %>%
    dplyr::mutate(track.mb.id = track.mb.id, track.ab.id = track.mb.id)
}
