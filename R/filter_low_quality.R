#' Filter low quality results for \emph{Discogs} Albums
#'
#' @param frame Data frame containing Discogs variabels.
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
                            c('album.dc.quality', 'artist.dc.quality'),
                            c(album_threshold, artist_threshold),
                            contentcols = discogsAlbumVars)
}


#' Filter low quality results for \emph{Genius} Tracks
#'
#' @param frame Data frame containing Genius variabels.
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
                            c('track.g.quality', 'artist.g.quality'),
                            c(track_threshold, artist_threshold),
                            contentcols = geniusLyricsVars)
}

#' Filter low quality results for \emph{Deezer} Tracks
#'
#' @param frame Data frame containing Deezer tracks variabels.
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
                            c('track.dz.quality', 'artist.dz.quality'),
                            c(track_threshold, artist_threshold),
                            contentcols = deezerTrackVars)
}

#' Filter low quality results for \emph{Deezer} Albums
#'
#' @param frame Data frame containing Deezer album variabels.
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
                            c('album.dz.quality', 'artist.dz.quality'),
                            c(album_threshold, artist_threshold),
                            contentcols = deezerAlbumVars)
}


#' Filter low quality results for \emph{Deezer} Artist
#'
#' @param frame Data frame containing Deezer artist variabels.
#' @param artist_threshold Threshold for Deezer artist quality. Ranging between 0 and 1.
#'
#' @return Updated input frame with all Deezer artist information set to NA where at least one threshold was not met.
#' @export
#'
#' @examples
filter_quality_deezer_artists <- function(frame, artist_threshold){
  if(! all(deezerArtistVars %in% colnames(frame))){
    stop('Please make sure the input frame contains all Deezer artist information by running get_albums_deezer first.')
  }
  filter_lowquality_content(frame,
                            c('artist.dz.quality'),
                            c(artist_threshold),
                            contentcols = deezerArtistVars)
}

#' #' Filter low quality results for \emph{Deezer} all (track) variables
#' #'
#' #' @param frame Data frame containing Deezer artist variabels.
#' #' @param artist_threshold Threshold for Deezer artist quality. Ranging between 0 and 1.
#' #'
#' #' @return Updated input frame with all Deezer artist information set to NA where at least one threshold was not met.
#' #' @export
#' #'
#' #' @examples
#' filter_quality_deezer_tracks <- function(frame, artist_threshold){
#'   if(! all(deezerArtistVars %in% colnames(frame))){
#'     stop('Please make sure the input frame contains all Deezer artist information by running get_albums_deezer first.')
#'   }
#'   filter_lowquality_content(frame,
#'                             c('artist.dz.quality'),
#'                             c(artist_threshold),
#'                             contentcols = deezerArtistVars)
#' }



filter_lowquality_content <- function(input, qualitycols, thresholds, contentcols){
  if(length(qualitycols) != length(thresholds)){
    stop('Please make sure that you pass  threshold for each quality vector so that qualitycols and thresholds are of equal length.')
  }
  contentcols <- union(contentcols, qualitycols)



  message(paste0('Filtering out data with low linking quality based on ',
                 paste(qualitycols), '...'))
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
