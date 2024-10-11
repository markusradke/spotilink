#' Filter low quality results for \emph{Discogs} Albums
#'
#' @param frame Data frame containing Discogs variables.
#' @param album_threshold Threshold for Discogs album quality. Ranging between 0 and 1.
#' @param artist_threshold Threshold for Discogs artist quality. Ranging between 0 and 1.
#'
#' @return Updated input frame with all Discogs information set to NA where at least one threshold was not met.
#' @export
#'
#' @examples
filter_quality_discogs_albums <- function(frame, album_threshold, artist_threshold){
  if(! all(discogsAlbumVars %in% colnames(frame))){
    stop('Please make sure the input frame contains all Discogs album information by running get_albums_discogs first.')
  }
  filter_lowquality_content(frame,
                            c('album.dc.quality', 'album.dc.firstartist.quality'),
                            c(album_threshold, artist_threshold),
                            contentcols = discogsAlbumVars)
}


#' Filter low quality results for \emph{Genius} Tracks
#'
#' @param frame Data frame containing Genius variables.
#' @param track_threshold Threshold for Genius track quality. Ranging between 0 and 1.
#' @param artist_threshold Threshold for Genius artist quality. Ranging between 0 and 1.
#'
#' @return Updated input frame with all Genius track information set to NA where at least one threshold was not met.
#' @export
#'
#' @examples
filter_quality_genius_tracks <- function(frame, track_threshold, artist_threshold){
  if(! all(geniusLyricsVars %in% colnames(frame))){
    stop('Please make sure the input frame contains all Genius track information by running get_tracks_genius first.')
  }
  filter_lowquality_content(frame,
                            c('track.g.quality', 'track.g.firstartist.quality'),
                            c(track_threshold, artist_threshold),
                            contentcols = geniusLyricsVars)
}

#' Filter low quality results for \emph{Deezer} Tracks
#'
#' @param frame Data frame containing Deezer tracks variables.
#' @param track_threshold Threshold for Deezer track quality. Ranging between 0 and 1.
#' @param artist_threshold Threshold for Deezer artist quality. Ranging between 0 and 1.
#'
#' @return Updated input frame with all Deezer track information set to NA where at least one threshold was not met.
#' @export
#'
#' @examples
filter_quality_deezer_tracks <- function(frame, track_threshold, artist_threshold){
  if(! all(deezerTrackVars %in% colnames(frame))){
    stop('Please make sure the input frame contains all Deezer track information by running get_tracks_deezer first.')
  }
  filter_lowquality_content(frame,
                            c('track.dz.quality', 'track.dz.firstartist.quality'),
                            c(track_threshold, artist_threshold),
                            contentcols = deezerTrackVars)
}

#' Filter low quality results for \emph{Deezer} Albums
#'
#' @param frame Data frame containing Deezer album variables.
#' @param album_threshold Threshold for Deezer album quality. Ranging between 0 and 1.
#' @param artist_threshold Threshold for Deezer artist quality. Ranging between 0 and 1.
#'
#' @return Updated input frame with all Deezer album information set to NA where at least one threshold was not met.
#' @export
#'
#' @examples
filter_quality_deezer_albums <- function(frame, album_threshold, artist_threshold){
  if(! all(deezerAlbumVars %in% colnames(frame))){
    stop('Please make sure the input frame contains all Deezer album information by running get_albums_deezer first.')
  }
  filter_lowquality_content(frame,
                            c('album.dz.quality', 'album.dz.firstartist.quality'),
                            c(album_threshold, artist_threshold),
                            contentcols = deezerAlbumVars)
}


#' Filter low quality results for \emph{Deezer} Artists
#'
#' @param frame Data frame containing Deezer artist variables.
#' @param artist_threshold Threshold for Deezer artist quality. Ranging between 0 and 1.
#'
#' @return Updated input frame with all Deezer artist information set to NA where at least one threshold was not met.
#' @export
#'
#' @examples
filter_quality_deezer_artists <- function(frame, artist_threshold){
  if(! all(deezerArtistVars %in% colnames(frame))){
    stop('Please make sure the input frame contains all Deezer artist information by running get_artists_deezer first.')
  }
  filter_lowquality_content(frame,
                            c('artist.dz.quality'),
                            c(artist_threshold),
                            contentcols = deezerArtistVars)
}

#' Filter low quality results for \emph{Deezer} all (track inferred) variables
#'
#' @param frame Data frame containing Deezer artist variables.
#' @param track_threshold Threshold for Deezer artist quality. Ranging between 0 and 1.
#' @param album_threshold Threshold for Deezer artist quality. Ranging between 0 and 1.
#' @param artist_threshold Threshold for Deezer artist quality. Ranging between 0 and 1.
#'
#' @return Updated input frame with all Deezer artist information set to NA where at least one threshold was not met.
#' @export
#'
#' @examples
filter_quality_deezer_all <- function(frame, track_threshold, album_threshold, artist_threshold){
  if(! all(deezerAllVars %in% colnames(frame))){
    stop('Please make sure the input frame contains all Deezer information by running get_all_deezer first.')
  }

  only_artist_content <- stringr::str_subset(deezerAllVars, 'firstartist')
  only_album_content <- stringr::str_subset(deezerAllVars, 'album')
  only_track_content <- stringr::str_subset(deezerAllVars, '(album|firstartist)', negate = T)

  result <- filter_lowquality_content(frame,
                                      c('track.dz.quality', 'track.dz.firstartist.quality'),
                                      c(track_threshold, artist_threshold),
                                      contentcols = only_track_content)
  result$track.dz.firstartist.quality <- ifelse(is.na(result$track.dz.firstartist.quality),
                                                0,
                                                result$track.dz.firstartist.quality)

  result <- filter_lowquality_content(result,
                            c('track.dz.album.quality', 'track.dz.firstartist.quality'),
                            c(album_threshold, artist_threshold),
                            contentcols = only_album_content)
  result$track.dz.firstartist.quality <- ifelse(is.na(result$track.dz.firstartist.quality),
                                                0,
                                                result$track.dz.firstartist.quality)

  result <- filter_lowquality_content(result,
                            c('track.dz.firstartist.quality'),
                            c(artist_threshold),
                            contentcols = only_artist_content)
  result
}


#' Filter low quality results for \emph{Musicbrainz} Artists
#'
#' @param frame Data frame containing Musicbrainz artist variables.
#' @param artist_threshold Threshold for Musicbrainz artist quality. Ranging between 0 and 1.
#'
#' @return Updated input frame with all Musicbrainz artist information set to NA where at least one threshold was not met.
#' @export
#'
#' @examples
filter_quality_musicbrainz_artists <- function(frame, artist_threshold){
  if(! all(musicbrainzArtistVars %in% colnames(frame))){
    stop('Please make sure the input frame contains all Musicbrainz artist information by running get_artists_musicbrainz first.')
  }
  filter_lowquality_content(frame,
                            c('artist.mb.quality'),
                            c(artist_threshold),
                            contentcols = musicbrainzArtistVars)
}


#' Filter low quality results for \emph{Musicbrainz} Albums
#'
#' @param frame Data frame containing Musicbrainz album variables.
#' @param album_threshold Threshold for Musicbrainz album quality. Ranging between 0 and 1.
#'
#' @return Updated input frame with all Musicbrainz album information set to NA where at least one threshold was not met.
#' @export
#'
#' @examples
filter_quality_musicbrainz_albums <- function(frame, album_threshold, artist_threshold){
  if(! all(musicbrainzAlbumVars %in% colnames(frame))){
    stop('Please make sure the input frame contains all Musicbrainz album information by running get_albums_musicbrainz first.')
  }
  filter_lowquality_content(frame,
                            c('album.mb.quality', 'album.mb.firstartist.quality'),
                            c(album_threshold, artist_threshold),
                            contentcols = musicbrainzAlbumVars)
}

#' Filter low quality results for \emph{Musicbrainz} and \emph{Acousticbrainz} Tracks
#'
#' @param frame Data frame containing Musicbrainz track variables.
#' @param track_threshold Threshold for Musicbrainz track quality. Ranging between 0 and 1.
#'
#' @return Updated input frame with all Musicbrainz and Acousticbrainz track information set to NA where at least one threshold was not met.
#' @export
#'
#' @examples
filter_quality_musicbrainz_acousticbrainz_tracks <- function(frame, track_threshold, artist_threshold){
  if(! all(musicbrainzTrackVars %in% colnames(frame))){
    stop('Please make sure the input frame contains all Musicbrainz track information by running get_tracks_musicbrainz first.')
  }
  if(!'track.ab.id' %in% colnames(frame)){
    result <- filter_lowquality_content(frame,
                              c('track.mb.quality', 'track.mb.firstartist.quality'),
                              c(track_threshold, artist_threshold),
                              contentcols = musicbrainzTrackVars)
  }
  else{
    if(! all(acousticbrainzTrackVars %in% colnames(frame))){
      stop('Please make sure the input frame contains all Acousticbrainz track information by running get_tracks_acousticbrainz first.')
    }
    result <- filter_lowquality_content(frame,
                            c('track.mb.quality', 'track.mb.firstartist.quality'),
                            c(track_threshold, artist_threshold),
                            contentcols = c(musicbrainzTrackVars, acousticbrainzTrackVars))

  }
  result
}

#' Filter low quality results for all \emph{Musicbrainz} Information
#'
#' @param frame Data frame containing all Musicbrainz variables.
#' @param track_threshold Threshold for Musicbrainz track quality. Ranging between 0 and 1.
#' @param album_threshold Threshold for Musicbrainz track quality. Ranging between 0 and 1.
#' @param artist_threshold Threshold for Musicbrainz track quality. Ranging between 0 and 1.
#'
#' @return Updated input frame with all Musicbrainz information set to NA where at least one threshold was not met. For album and track, artist quality will also be considered.
#' @export
#'
#' @examples
filter_quality_musicbrainz_all <- function(frame, track_threshold, album_threshold, artist_threshold){
  frame %>%
    filter_quality_musicbrainz_acousticbrainz_tracks(track_threshold) %>%
    filter_quality_musicbrainz_albums(album_threshold) %>%
    filter_quality_musicbrainz_artists(artist_threshold) %>%
    combine_genres_musicbrainz()
}



filter_lowquality_content <- function(input, qualitycols, thresholds, contentcols){
  if(length(qualitycols) != length(thresholds)){
    stop('Please make sure that you pass  threshold for each quality vector so that qualitycols and thresholds are of equal length.')
  }
  contentcols <- union(contentcols, qualitycols)


  message(paste0('Filtering out data with low linking quality based on ',
                 toString(qualitycols), '...'))
  res <- input
  res$keepdata <- TRUE
  for(i in seq(1:length(qualitycols))){
    res <- res %>%
      dplyr::mutate(keepdata = ifelse(.data[[qualitycols[i]]] >= thresholds[i], keepdata, FALSE))
  }
  res <- res %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(contentcols), ~ ifelse(keepdata, .x, NA))) %>%
    dplyr::select(-keepdata)
  message('Done.')
  res
}
