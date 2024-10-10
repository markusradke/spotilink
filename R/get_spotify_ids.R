#' Search \emph{Spotify} for IDs
#'
#' Searches for either tracks, albums, or artists \emph{Spotify} IDs, depending on the input supplied. Artist names must always be supplied and artist.s.ids will always be returned.
#' If additional track titles are supplied, track.s.ids will be returned. If additional album title are supplied, album.s.ids will be returned. If tracks and albums are supplied the album information is used to find the track in the corresponding album. If release years are supplied, the information is used to find the best suiting track or album.  If you want to retrieve both tracks and album ids, please first look for track.s.ids and then use [get_all_spotify()] to retrieve the album.s.ids.
#'
#' @param artists Character vector with artist names
#' @param pass Character Vector containing two entries: \emph{Client ID} and \emph{Client secret}. See \url{https://developer.spotify.com/documentation/web-api/concepts/authorization} for details.
#' @param tracks Optional character vector with  track titles. Must be the same length as the artist vector.
#' @param albums Optional character vector with  album titles. Must be the same length as the artist vector.
#' @param albums Optional character vector with  release years. Must be the same length as the artist vector. Must be combined with a track vector and / or an album vector.
#' @param artist_threshold Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
#'The values correspond to the string similarity (1 - Jaro-Winkler distance) between the searched artist / track / album and the found name or title on \emph{Spotify}. For tracks and albums, \emph{spotilink} will only keep results where the artist name as well as the track / album title surpass the threshold.
#' @param track_or_album_threshold Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
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
get_spotify_ids <- function(artists, pass, tracks = NA, albums = NA, releaseyear = NA, artist_threshold = 0.8, track_or_album_threshold = 0.8){
  check_assertions_get_spotify_ids(artists, tracks, albums, releaseyear, artist_threshold, track_or_album_threshold)
  connect_spotify(pass)


  input <- data.frame(artists = artists, tracks = tracks, albums = albums, releaseyear = releaseyear) %>%
    dplyr::distinct(artists, tracks, albums, .keep_all = T)
  if(! all(is.na(input$tracks))){
    res <- search_tracks_on_spotify(input, artist_threshold, track_or_album_threshold)
  }
  else{
    if(! all(is.na(input$albums))){
      res <- search_albums_on_spotify(input, artist_threshold, track_or_album_threshold)
    }
    else{
      res <- search_artists_on_spotify(input, artist_threshold)
    }
  }

  print_spotify_id_linkage(res)
  res
}

check_assertions_get_spotify_ids <- function(artists, tracks, albums, releaseyear, artist_threshold, track_or_album_threshold){
  if(any(is.na(artists)) | class(artists) != 'character'){
    stop('Please make sure the artists vector is a character vector without NAs.')
  }
  if(length(tracks) > 1 & (any(is.na(tracks)) | class(tracks) != 'character')){
    stop('Please make sure the tracks vector is a character vector without NAs.')
  }
  if(length(tracks) == 1){
    if((! class(tracks) %in% c('character', 'logical') & ! is.na(tracks))){
      stop('Please make sure the tracks vector is a character vector without NAs.')
    }
  }
  if(length(albums) > 1 & (any(is.na(albums)) | class(albums) != 'character')){
    stop('Please make sure the albums vector is a character vector without NAs.')
  }
  if(length(albums) == 1) {
    if((! class(albums) %in% c('character', 'logical') & ! is.na(albums))){
      stop('Please make sure the albums vector is a character vector without NAs.')
    }
  }
  if(!all(is.na(tracks)) & length(tracks) != length(artists)){
    stop('Please make sure the artists vector and the optional track / album / release year vectors are all vectors of the same length.')
  }
  if(!all(is.na(albums)) & length(albums) != length(artists)){
    stop('Please make sure the artists vector and the optional track / album / release year vectors are all vectors of the same length.')
  }
  if(!all(is.na(releaseyear)) & length(releaseyear) != length(artists)){
    stop('Please make sure the artists vector and the optional track / album / release year vectors are all vectors of the same length.')
  }

  if(!all(is.na(releaseyear)) & all(is.na(tracks)) & all(is.na(albums))){
    stop('Please provide a track or album vector when specifiing releaseyears.')
  }

  if(length(artist_threshold) != 1 | class(artist_threshold) != 'numeric'){
    stop('Please make sure the threshold is a single number between 0 and 1.')
  }
  if(artist_threshold < 0 | artist_threshold > 1){
    stop('Please make sure the threshold is a single number between 0 and 1.')
  }
  if(length(track_or_album_threshold) != 1 | class(track_or_album_threshold) != 'numeric'){
    stop('Please make sure the threshold is a single number between 0 and 1.')
  }
  if(track_or_album_threshold < 0 | track_or_album_threshold > 1){
    stop('Please make sure the threshold is a single number between 0 and 1.')
  }
}


search_tracks_on_spotify <- function(input, artist_threshold, track_or_album_threshold){
  checkpoint_name <- 'spotify_search'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  saved_data <- checkpoint$saved_data
  if(last_index > 0) {input <- tail(input, -last_index)}
  purrr::pmap_df(list(input$artists, input$tracks, input$albums, input$releaseyear),
                        search_single_track_on_spotify %>% save_checkpoint_and_count(checkpoint_name, last_index, saved_data),
                 artist_threshold, track_or_album_threshold,
                        .progress = 'Looking up tracks IDs on Spotify...')
  res <- suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
  message('Done.')
  save_file_and_remove_checkpoints(res, checkpoint_name)
  res
}

search_albums_on_spotify <- function(input, artist_threshold, track_or_album_threshold){
  checkpoint_name <- 'spotify_search'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  saved_data <- checkpoint$saved_data
  if(last_index > 0) {input <- tail(input, -last_index)}
  purrr::pmap_df(list(input$artists, input$albums, input$releaseyear),
                 search_single_album_on_spotify %>% save_checkpoint_and_count(checkpoint_name, last_index, saved_data),
                 artist_threshold, track_or_album_threshold,
                 .progress = 'Looking up albums IDs on Spotify...')
  res <- suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
  message('Done.')
  save_file_and_remove_checkpoints(res, checkpoint_name)
  res
}

search_artists_on_spotify <- function(input, artist_threshold){
  checkpoint_name <- 'spotify_search'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  saved_data <- checkpoint$saved_data
  if(last_index > 0) {input <- tail(input, -last_index)}
  purrr::map_df(input$artists,
               search_single_artist_on_spotify %>% save_checkpoint_and_count(checkpoint_name, last_index, saved_data),
               artist_threshold, .progress = 'Looking up artists IDs on Spotify...')
  res <- suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
  message('Done.')
  save_file_and_remove_checkpoints(res, checkpoint_name)
  res
}

search_single_track_on_spotify <- function(artist, track, album, releaseyear, artist_threshold, track_threshold){
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
                    track.s.quality = stringdist::stringsim(simplify_name(track.s.title), simplify_name(track.search), 'jw'),
                    artist.s.quality = stringdist::stringsim(simplify_name(artist.s.name), simplify_name(artist.search), 'jw'),
                    album.s.quality = stringdist::stringsim(simplify_name(album), simplify_name(album.name), 'jw'),
                    album.s.releaseyear = stringr::str_sub(album.release_date, start = 1, 4) %>% as.integer(),
                    releasediff = abs(album.s.releaseyear - releaseyear)) %>%
      dplyr::arrange(-artist.s.quality, -track.s.quality, -album.s.quality, releasediff, -popularity) %>%
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
  if(!is.na(album)){query <- paste0('artist:', artist, ' track:',track, ' album:',album)}
  else{query <- paste0('artist:', artist, ' track:',track)}

  res <- spotifyr::search_spotify(query, type = 'track')
  if(nrow(res) == 0){return(.make_empty_frame())}
  res <- .parse_results(res)
  if(res$artist.s.quality < artist_threshold | res$track.s.quality < track_threshold) {return(.make_empty_frame())}
  res
}

search_single_album_on_spotify <- function(artist, album, releaseyear, artist_threshold, album_threshold){
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
                    artist.s.quality = 1 - stringdist::stringdist(simplify_name(artist.s.name), simplify_name(artist.search), 'jw'),
                    album.s.releaseyear = stringr::str_sub(release_date, start = 1, 4) %>% as.integer(),
                    releasediff = abs(album.s.releaseyear - releaseyear)) %>%
      dplyr::arrange(-artist.s.quality, -album.s.quality, releasediff, -total_tracks) %>%
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
  if(res$artist.s.quality < artist_threshold | res$album.s.quality < album_threshold) {return(.make_empty_frame())}
  res
}

search_single_artist_on_spotify <- function(artist, threshold){
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

