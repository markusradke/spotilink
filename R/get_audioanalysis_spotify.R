#'Get \emph{Spotify} API Audio Analysis for Tracks
#'
#'Retrieve a data frame containing information from the \emph{Spotify} API.
#'The result contains tracks' audio analysis using the  \pkg{spotilink} naming convention.
#'
#' @param input
#'Data Frame containing a column \code{track.s.id} with \emph{Spotify} track ids.
#' @param pass
#'Character Vector containing two entries: \emph{Client ID} and \emph{Client secret}. See \url{https://developer.spotify.com/documentation/web-api/concepts/authorization} for details.
#'
#' @return Data Frame with added information from the \emph{Spotify} Web API using the \pkg{spotilink} naming convention. For retrieving further metadata on tracks, please use the \code{\link{get_tracks_spotify}} function.
#' @export
#'
#'@examples
get_audioanalysis_spotify <- function(input, pass) {
  connect_spotify(pass)
  # res <- rename_existing_variables(input, 'spotify_audioanalysis') #TODO implement single renaming requests
  # get_from_API(res, 'track.s.id', get_track_audio_analysis, clean_analysis, batchsize = 1) # TODO later
}


clean_analysis <- function(analysisRaw) {
  dplyr::tibble(meta = list(analysisRaw$meta),
                track = list(analysisRaw$track),
                bars = list(analysisRaw$bars),
                beats = list(analysisRaw$beats),
                sections = list(analysisRaw$sections),
                segments = list(analysisRaw$segments),
                tatums = list(analysisRaw$tatums)) %>%
    dplyr::mutate(track.s.id = 'x') %>% # todo incorporate id and make data frame
    dplyr::select('track.s.id', dplyr::everything())
}
