#'Get Naming Convention
#'
#'Retrieve a character vector with the names of all variables that can be obtained using \pkg{spotilink}.
#'
#' @return Character vector with variable names.
#' @export
#'
#'@examples
#'get_naming_convention()
get_naming_convention <- function() {
  c(spotifyAllVars, musicbrainzAllVars, spotifyAudioanalysisVars,
    discogsAlbumVars, geniusLyricsVars, deezerAllVars) %>% unique()
}
