#'Get \emph{MusicBrainz} Information
#'
#'Retrieve a data frame containing information from the \emph{MusicBrainz} API.
#'The result contains information on the tracks as well as corresponding albums and artists.
#'The \pkg{spotilink} naming convention is used.
#'
#' @param input
#'Data Frame containing the following columns:
#'\itemize{
#'  \item\code{track.s.id} \cr
#'  with \emph{Spotify} track ids,
#'  \item \code{track.s.title} \cr
#'  with \emph{Spotify} track name,
#'  \item \code{track.s.isrc} \cr
#'  with track ISRCs from \emph{Spotify},
#'  \item \code{track.s.firstartist.name} \cr
#'  with the track's first artist name from \emph{Spotify},
#'  \item \code{album.s.id} \cr
#'  with \emph{Spotify} album id,
#'  \item \code{album.s.title} \cr
#'  with \emph{Spotify} album title,
#'  \item \code{album.s.upc} \cr
#'  with album UPCs from \emph{Spotify}.
#'  \item \code{artist.s.id} \cr
#'  with \emph{Spotify} artist id,
#'  \item \code{artist.s.name} \cr
#'  with \emph{Spotify} artist name.
#'}
#'It is advisable to first run \code{\link{get_all_spotify}} before running this command,
#'in order to have all the necessary information.
#'
#'@param track_threshold,album_threshold,artist_threshold
#'Floating point numbers between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
#'The values correspond to the string similarity (1 - Jaro-Winkler distance) between
#'\enumerate{
#'  \item the track title on \emph{Spotify} and the found track title on \emph{Musicbrainz}.
#'  \item the album title on \emph{Spotify} and the found album title on \emph{Musicbrainz}.
#'  \item the artist name on \emph{Spotify} and the found artist name on \emph{Musicbrainz}. It also is used to filter low quality entries for tracks and albums as the threshold for the name of the tracks or albums first artist.
#'}
#'
#' @return Data Frame with added information from the \emph{MusicBrainz} API using the  \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
#'data <- data.frame(track.s.id = c('4ZXLWTmQFzM02hZwMiZfgS'),
#'                   track.s.title = c('Der Geist hilft unser Schwachheit auf, BWV 226'),
#'                   track.s.isrc = c('GBHNG1200003'),
#'                   track.s.firstartist.name = c('Johann Sebastian Bach'),
#'                   album.s.id = c('2rlWvQ1GuTOSkyNpmcoaMC'),
#'                   album.s.title = c('Bach: Motets'),
#'                   album.s.upc = c('843183071623'),
#'                   artist.s.id = c('5aIqB5nVVvmFsvSdExz408'),
#'                   artist.s.name = c('Johann Sebastian Bach'))
#'
#'get_all_musicbrainz(data)
get_all_musicbrainz <- function(input, track_threshold = 0.8, album_threshold = 0.8, artist_threshold = 0.8) {
  are_needed_columns_present(input, c('track.s.id',
                                      'track.s.title',
                                      'track.s.isrc',
                                      'track.s.firstartist.name',
                                      'album.s.id',
                                      'album.s.title',
                                      'album.s.upc',
                                      'artist.s.id',
                                      'artist.s.name'))
  res <- rename_existing_variables(input, musicbrainzAllVars)

  if(file.exists('mb_tracks.rds')){
    message('Checkpoint detected. Track retrieval already done.')
    res <- readRDS('mb_tracks.rds')
  }
  else{res <- pull_tracks_musicbrainz(res, track_threshold, artist_threshold)}
  if(file.exists('mb_albums.rds')){
    message('Checkpoint detected. Album retrieval already done.')
    res <- readRDS('mb_albums.rds')
  }
  else{res <- pull_albums_musicbrainz(res, album_threshold, artist_threshold)}
  if(file.exists('mb_artists.rds')){
    message('Checkpoint detected. Artist retrieval already done.')
    res <- readRDS('mb_artists.rds')
  }
  else{res <- pull_artists_musicbrainz(res, artist_threshold)}

  res <- combine_genres_musicbrainz(res)
  print_linking_success(res, c('track.s.id', 'album.s.id', 'artist.s.id'))
  saveRDS(res, 'musicbrainz_all.rds')
  musicbrainz_all_remove_checkpoints()
  res
}

combine_genres_musicbrainz <- function(input) {
  input %>%
    dplyr::mutate(track.mb.combinedgenre = ifelse(!is.na(.data[['track.mb.topgenre']]), .data[['track.mb.topgenre']], .data[['album.mb.topgenre']])) %>%
    dplyr::mutate(track.mb.combinedgenre = ifelse(!is.na(.data[['track.mb.combinedgenre']]), .data[['track.mb.combinedgenre']], .data[['artist.mb.topgenre']])) %>%
    dplyr::mutate(album.mb.combinedgenre = ifelse(!is.na(.data[['album.mb.topgenre']]), .data[['album.mb.topgenre']], .data[['track.mb.topgenre']])) %>%
    dplyr::mutate(album.mb.combinedgenre = ifelse(!is.na(.data[['album.mb.combinedgenre']]), .data[['album.mb.combinedgenre']], .data[['artist.mb.topgenre']])) %>%
    dplyr::mutate(artist.mb.combinedgenre = ifelse(!is.na(.data[['artist.mb.topgenre']]), .data[['artist.mb.topgenre']], .data[['album.mb.topgenre']])) %>%
    dplyr::mutate(artist.mb.combinedgenre = ifelse(!is.na(.data[['artist.mb.combinedgenre']]), .data[['artist.mb.combinedgenre']], .data[['track.mb.topgenre']]))
}


musicbrainz_all_remove_checkpoints <- function(){
  pattern <- paste0('^mb_(albums|tracks|artists).rds$')
  files <- list.files()
  matching_files <- grep(pattern, files, value = TRUE)
  for(file in matching_files){file.remove(file)}
}
