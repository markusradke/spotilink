#'Get \emph{Spotify} API Information on Artists
#'
#'Retrieve a data frame containing information from the \emph{Spotify} API.
#'The result contains information on the artists using the  \pkg{spotilink} naming convention.
#'
#' @param input
#'Data Frame containing a column \code{artist.s.id} with \emph{Spotify} artist ids.
#' @param pass
#'Character Vector containing two entries: \emph{Client ID} and \emph{Client secret}. See \url{https://developer.spotify.com/documentation/web-api/concepts/authorization} for details.
#'
#' @return Data Frame with added information from the \emph{Spotify} Web API using the \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
get_artists_spotify <- function(input, pass) {
  are_needed_columns_present(input, c('artist.s.id'))
  renameVars <- spotifyArtistVars[! spotifyArtistVars %in% c('artist.s.id')]
  res <- rename_existing_variables(input, renameVars)

  connect_spotify(pass)
  pull_artists_spotify(res)
}

pull_artists_spotify <- function(input) {
  res <- input
  if ('artist.s.name' %in% colnames(res)) { # remove redundant artist name from get tracks if necessary
    res <- dplyr::select(res, -'artist.s.name')
  }
  get_from_API(res, 'artist.s.id', spotifyr::get_artists, clean_artists, batchsize = 50)
}

clean_artists <- function(artistsRaw) {
  artistsRaw %>%
    dplyr::select('artist.s.id' = 'id',
                  'artist.s.name' = 'name',
                  'artist.s.genres' = 'genres',
                  'artist.s.popularity' = 'popularity',
                  'artist.s.followers' = 'followers.total') %>%
    tidyr::hoist('artist.s.genres', artist.s.topgenre = list('genre', 1L), .remove = FALSE)
}
