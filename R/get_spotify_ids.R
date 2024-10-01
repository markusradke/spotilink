#' Search \emph{Spotify} for IDs
#'
#' Searches for either tracks, albums, or artists \emph{Spotify} IDs, depending on the input supplied. Artist names must always be supplied and artist.s.ids will always be returned.
#' If additional track titles are supplied, track.s.ids will be returned. If additional album title are supplied, album.s.ids will be returned. Albums and track ids cannot be searched for at the same time. If you want to retrieve both, please first look for track.s.ids and then use [get_all_spotify()] to retrieve the album.s.ids.
#'
#' @param artists Character vector with artist names
#' @param pass Character Vector containing two entries: \emph{Client ID} and \emph{Client secret}. See \url{https://developer.spotify.com/documentation/web-api/concepts/authorization} for details.
#' @param tracks Optional character vector with  track titles. Must be the same length as the artist vector. Cannot be combined with an album vector.
#' @param albums Optional character vector with  album titles. Must be the same length as the artist vector. Cannot be combined with a track vector.
#' @param threshold Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
#'The values correspond to the string similarity (1 - Jaro-Winkler distance) between the searched artist / track / album and the found name or title on \emph{Spotify}. For tracks and albums, \emph{spotilink} will only keep results where the artist name as well as the track / album title surpass the threshold.
#'
#' @return Data frame with search strings, \emph{Spotify} ids, found title / names and the calculated string similarity.
#' @export
#'
#' @examples
#' # get artist ids
#' get_spotify_ids(c('Nina Hagen', 'Olivia Rodrigo'), pass = s_pass)
#' # get track ids
#' get_spotify_ids(c('Nina Hagen', 'Olivia Rodrigo'), tracks = c('TV GLOTZER', 'drivers license'), pass = s_pass)
#' # get album ids
#' get_spotify_ids(c('Nina Hagen', 'Olivia Rodrigo'), albums = c('Nunsexmonkrock', 'SOUR'), pass = s_pass)
get_spotify_ids <- function(artists, pass, tracks = character(), albums = character(), threshold = 0.8){
  check_assertions_get_spotify_ids(artists, tracks, albums, threshold)
  connect_spotify(pass)

  if(length(tracks != 0)){
    input <- data.frame(artists = artists, tracks = tracks) %>% dplyr::distinct(artists, tracks)
    res <- purrr::map2_df(input$artists, input$tracks, search_track_on_spotify, threshold, .progress = 'Looking up tracks IDs on Spotify...')
  }
  else{
    if(length(albums != 0)){
      input <- data.frame(artists = artists, albums = albums) %>% dplyr::distinct(artists, albums)
      res <- purrr::map2_df(input$artists, input$albums, search_album_on_spotify, threshold, .progress = 'Looking up albums IDs on Spotify...')
    }
    else{
      artist <- unique(artists)
      res <- purrr::map_df(artists, search_artist_on_spotify, threshold, .progress = 'Looking up artists IDs on Spotify...')
    }
  }

  print_spotify_id_linkage(res)
  res
}

check_assertions_get_spotify_ids <- function(artists, tracks, albums, threshold){
  if(class(artists) != 'character'){
    stop('Please make sure the artists vector and the optional track / album vector are character vectors of the same length.')
  }
  if(class(tracks) != 'character'){
    stop('Please make sure the artists vector and the optional track / album vector are character vectors of the same length.')
  }
  if(class(albums) != 'character'){
    stop('Please make sure the artists vector and the optional track / album vector are character vectors of the same length.')
  }
  if(length(tracks) != 0 & length(tracks) != length(artists)){
    stop('Please make sure the artists vector and the optional track / album vector are character vectors of the same length.')
  }
  if(length(albums) != 0 & length(albums) != length(artists)){
    stop('Please make sure the artists vector and the optional track / album vector are character vectors of the same length.')
  }
  if(length(tracks) != 0 & length(albums) != 0){
    stop('Please provide either track or album together with the artist to search for the corresponding type.')
  }
  if(length(threshold) != 1 | class(threshold) != 'numeric'){
    stop('Please make sure the threshold is a single number between 0 and 1.')
  }
  if(threshold < 0 | threshold > 1){
    stop('Please make sure the threshold is a single number between 0 and 1.')
  }
}


search_track_on_spotify <- function(artist, track, threshold){
  .make_empty_frame <- function(){
    data.frame(artist.search = artist,
               track.search = track,
               track.s.id = NA,
               track.s.title = NA,
               track.s.quality = NA,
               artist.s.id = NA,
               artist.s.name = NA,
               artist.s.quality = NA)
  }

  .parse_results <- function(res){
    res %>%
      tidyr::hoist(artists, artist.s.name = list('name', 1L), .remove = F) %>%
      tidyr::hoist(artists, artist.s.id = list('id', 1L), .remove = F) %>%
      dplyr::rename(track.s.title = name,
                    track.s.id = id) %>%
      dplyr::mutate(artist.search = artist,
                    track.search = track,
                    track.s.quality = 1 - stringdist::stringdist(simplify_name(track.s.title), simplify_name(track.search), 'jw'),
                    artist.s.quality = 1 - stringdist::stringdist(simplify_name(artist.s.name), simplify_name(artist.search), 'jw')) %>%
      dplyr::arrange(-artist.s.quality, -track.s.quality, -popularity) %>%
      dplyr::first() %>%
      dplyr::select(artist.search,
                    track.search,
                    track.s.id,
                    track.s.title,
                    track.s.quality,
                    artist.s.id,
                    artist.s.name,
                    artist.s.quality)
  }
  query = paste0('artist:', artist, ' track:',track)
  res <- spotifyr::search_spotify(query, type = 'track')
  if(nrow(res) == 0){return(.make_empty_frame())}
  res <- .parse_results(res)
  if(res$artist.s.quality < threshold | res$track.s.quality < threshold) {return(.make_empty_frame())}
  res
}

search_album_on_spotify <- function(artist, album, threshold){
  .make_empty_frame <- function(){
    data.frame(artist.search = artist,
               album.search = album,
               album.s.id = NA,
               album.s.title = NA,
               album.s.quality = NA,
               artist.s.id = NA,
               artist.s.name = NA,
               artist.s.quality = NA)
  }
  .parse_results <- function(res){
    res %>%
      tidyr::hoist(artists, artist.s.name = list('name', 1L), .remove = F) %>%
      tidyr::hoist(artists, artist.s.id = list('id', 1L), .remove = F) %>%
      dplyr::rename(album.s.title = name,
                    album.s.id = id) %>%
      dplyr::mutate(artist.search = artist,
                    album.search = album,
                    album.s.quality = 1 - stringdist::stringdist(simplify_name(album.s.title), simplify_name(album.search), 'jw'),
                    artist.s.quality = 1 - stringdist::stringdist(simplify_name(artist.s.name), simplify_name(artist.search), 'jw')) %>%
      dplyr::arrange(-artist.s.quality, -album.s.quality, -total_tracks) %>%
      dplyr::first() %>%
      dplyr::select(artist.search,
                    album.search,
                    album.s.id,
                    album.s.title,
                    album.s.quality,
                    artist.s.id,
                    artist.s.name,
                    artist.s.quality)
  }

  query = paste0('artist:', artist, ' album:',album)
  res <- spotifyr::search_spotify(query, type = 'album')
  if(nrow(res) == 0){return(.make_empty_frame())}
  res <- .parse_results(res)
  if(res$artist.s.quality < threshold | res$album.s.quality < threshold) {return(.make_empty_frame())}
  res
}

search_artist_on_spotify <- function(artist, threshold){
  .make_empty_frame <- function(){
    data.frame(artist.search = artist,
               artist.s.id = NA,
               artist.s.name = NA,
               artist.s.quality = NA)
  }
  .parse_results <- function(res){
    res %>%
      dplyr::mutate(artist.search = artist,
                    artist.s.quality = 1 - stringdist::stringdist(simplify_name(name), simplify_name(artist), 'jw')) %>%
      dplyr::arrange(-artist.s.quality, -popularity) %>%
      dplyr::first() %>%
      dplyr::select(artist.search,
                    artist.s.id = id,
                    artist.s.name = name,
                    artist.s.quality)
  }
  res <- spotifyr::search_spotify(artist, type = 'artist')
  if(nrow(res) == 0){return(.make_empty_frame())}
  res <- .parse_results(res)
  if(res$artist.s.quality < threshold) {return(.make_empty_frame())}
  res
}

print_spotify_id_linkage <- function(res){
  relfreq_na <- nrow(dplyr::filter(res, ! is.na(artist.s.id))) / nrow(res)
  relfreq_na_percent <- 100 * round(relfreq_na, 4)
  if('track.s.id' %in% colnames(res)){
    message(paste0('Done. Found ', relfreq_na_percent, '% of distinct tracks in the input data.')); return(NULL)
  }
  if('album.s.id' %in% colnames(res)){
    message(paste0('Done. Found ', relfreq_na_percent, '% of distinct albums in the input data.')); return(NULL)
  }
  message(paste0('Done. Found ', relfreq_na_percent, '% of distinct artists in the input data.'))
}

