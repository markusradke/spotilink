#' Select \emph{Spotilink}-Features from a data frame with results.
#'
#' @param input Input result data frame after
#' @param track_identifier,artist_identifier,release_identifier,label_identifier,linkage_variables,artist_descriptors,release_descriptors,spotify_highlevel_audio_descriptors,ml_highlevel_audio_classificators,ml_highlevel_audio_descriptors,audio_track_metadata,aggregated_track_lowlevel_features,genre_descriptors,ml_genre_classificators,genre_tags,platform_popularity,explicitness_and_lyrics True/False-flags for the inclusion of feature sets in the resulting data frame. Please refer to the feature documention sheet on GitHub for further details.Wil always include the track identifier \emph{track.s.id}.
#'
#' @return Data frame with selection of features specified through the parameter flags.
#' @export
#'
#' @examples select_features(testresults, track_identifier = T)
select_features <- function(input,
                            track_identifier = F,
                            artist_identifier = F,
                            release_identifier = F,
                            label_identifier = F,
                            linkage_variables = F,
                            artist_descriptors = F,
                            release_descriptors = F,
                            platform_highlevel_audio_descriptors = F,
                            ml_highlevel_audio_classificators = F,
                            ml_highlevel_audio_descriptors = F,
                            audio_track_metadata = F,
                            aggregated_track_lowlevel_features = F,
                            genre_descriptors = F,
                            ml_genre_classificators = F,
                            genre_tags = F,
                            platform_popularity = F,
                            cover_lyrics_explicitness = F){

  features <- c('track.s.id')
  if(track_identifier){features <- c(features, trackIdentifier)}
  if(artist_identifier){features <- c(features, artistIdentifier)}
  if(release_identifier){features <- c(features, releaseIdentifier)}
  if(label_identifier){features <- c(features, labelIdentifier)}
  if(linkage_variables){features <- c(features, linkageVariables)}
  if(artist_descriptors){features <- c(features, artistDescriptors)}
  if(release_descriptors){features <- c(features, releaseDescriptors)}
  if(platform_highlevel_audio_descriptors){features <- c(features, platformHighlevelAudioDescriptors)}
  if(ml_highlevel_audio_classificators){features <- c(features, mlHighlevelAudioClassificators)}
  if(ml_highlevel_audio_descriptors){features <- c(features, mlHighlevelAudioDescriptors)}
  if(audio_track_metadata){features <- c(features, audioTrackMetadata)}
  if(aggregated_track_lowlevel_features){features <- c(features, aggregatedTrackLowlevelFeatures)}
  if(genre_descriptors){features <- c(features, genreDescriptors)}
  if(ml_genre_classificators){features <- c(features, mlGenreClassificators)}
  if(genre_tags){features <- c(features, genreTags)}
  if(platform_popularity){features <- c(features, platformPopularity)}
  if(cover_lyrics_explicitness){features <- c(features, coverLyricsExplicitness)}

  features <- unique(features)
  features <- features[features %in% colnames(input)]

  input %>% dplyr::select(all_of(features))
}
