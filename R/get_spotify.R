#'Get \emph{Spotify} API Information
#'
#'Retrieve a data frame containing information from the \emph{Spotify} API.
#'The result contains information on the tracks as well as corresponding albums and artists.
#'The \pkg{spotilink} naming convention is used.
#'
#' @param input
#'Data Frame containing a column \code{track.s.id} with \emph{Spotify} track ids.
#' @param pass
#'Character Vector containing two entries: \emph{Client ID} and \emph{Client secret}. See \url{https://developer.spotify.com/documentation/web-api/concepts/authorization} for details.
#'
#' @return Data Frame with added information from the \emph{Spotify} Web API using the \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
get_spotify <- function(input, pass) {
  connect_spotify(pass)

  res <- rename_existing_variables(input, 'spotify')
  res <- get_from_API(res, 'track.s.id', spotifyr::get_tracks, clean_tracks, batchsize = 50)
  res <- get_from_API(res, 'track.s.id', spotifyr::get_track_audio_features, clean_features, batchsize = 50)
  res <- get_from_API(res, 'album.s.id', spotifyr::get_albums, clean_albums, batchsize = 20)
  res <- expand_artists(res)
  res <- get_from_API(res, 'artist.s.id', spotifyr::get_artists, clean_artists, batchsize = 50)
  # res <- get_from_API(res, 'track.s.id', get_track_audio_analysis, clean_analysis, batchsize = 1) # TODO later
  cat('Done.\n')
  res
}

connect_spotify<-function(pass){
  cat('Connecting with Spotify-API...', '\n')
  Sys.setenv(SPOTIFY_CLIENT_ID = pass[1])
  Sys.setenv(SPOTIFY_CLIENT_SECRET = pass[2])
  cat('Done.\n')
}

get_from_API <- function(input, IDcol, pullFunction, cleanFunction, batchsize) {
  cat('Retrieving data with', substitute(pullFunction) %>% as.character(), 'from identifier', IDcol, '... \n')
  input %>%
    dplyr::distinct(.data[[IDcol]]) %>%
    dplyr::pull(IDcol) %>%
    get_from_IDs(pullFunction, batchsize) %>%
    cleanFunction() %>%
    dplyr::left_join(input, ., by = IDcol)
}

get_from_IDs <- function(ids, pullFunction, batchsize) {
  total <- length(ids)
  pulled <- c()
  start <- 1
  stepsize <- batchsize

  repeat{
    stop <- min(c(start + stepsize - 1, total))
    cat("Getting infos for ", start ,"-", stop, " of ", total, "...\n")
    pulled <- pullFunction(ids[start:stop]) %>%
      rbind(pulled, .)
    start <- start + stepsize
    if (start > total) {
      break
    }
  }
  pulled
}

clean_tracks <- function(tracksRaw) {
  tracksRaw %>%
    # tidyr::hoist('artists', track.s.firstartist.id = list('id', 1L), .remove = FALSE) %>%
    # dplyr::mutate(track.s.artists = sapply(tracksRaw$artists,'[[','name')) %>%
    dplyr::mutate(name = .data[['name']]) %>%
    dplyr::mutate(album.name = .data[['album.name']]) %>%
    dplyr::mutate(track.s.duration = .data[['duration_ms']] * 0.001) %>%
    tidyr::hoist('artists', track.s.firstartist.id = list('id', 1L), .remove = FALSE) %>%
    tidyr::hoist('artists', track.s.firstartist.name = list('name', 1L), .remove = FALSE) %>%
    dplyr::rename('track.s.artists' = 'artists') %>%
    dplyr::select(
      'track.s.id' = 'id',
      'track.s.title' = 'name',
      'track.s.artistlist' = 'track.s.artists',
      'track.s.firstartist.id',
      'track.s.firstartist.name',
      'track.s.artists',
      'track.s.explicit' = 'explicit',
      'track.s.popularity' = 'popularity',
      'track.s.isrc' = 'external_ids.isrc',
      'track.s.duration_ms' = 'duration_ms',
      'track.s.duration',
      'track.s.popularity' = 'popularity',
      'track.s.albumPosition' = 'track_number',
      'album.s.id' = 'album.id',
      'album.s.title' = 'album.name'
    )
}

clean_features <- function(featuresRaw){
  keyLookup <- c('C', 'C#', 'D', 'Eb', 'E', 'F', 'F#', 'G', 'G#', 'A', 'Bb', 'B', 'no')
  featuresRaw %>%
    dplyr::mutate(time_signature = .data[['time_signature']] %>% as.character() %>% stringr::str_c('/4') %>% as.factor()) %>%
    dplyr::mutate(key = keyLookup[.data[['key']] + 1] %>% as.factor()) %>% # +1 due to 1-indexing in R vs. 0-indexing of keys in API
    dplyr::mutate(mode = ifelse(.data[['mode']] == 1, 'major', 'minor') %>% as.factor()) %>%
    dplyr::select('track.s.id' = 'id',
           'track.s.danceability' = 'danceability',
           'track.s.energy' = 'energy',
           'track.s.key' = 'key',
           'track.s.loudness' = 'loudness',
           'track.s.mode' = 'mode',
           'track.s.speechiness' = 'speechiness',
           'track.s.acousticness' = 'acousticness',
           'track.s.instrumentalness' = 'instrumentalness',
           'track.s.liveness' = 'liveness',
           'track.s.valence' = 'valence',
           'track.s.tempo' = 'tempo',
           'track.s.time_signature' = 'time_signature')
}

clean_albums <- function(albumsRaw){
  albumsRaw %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(album.s.release_year = stringr::str_sub(.data[['release_date']], end = 4) %>% as.integer()) %>%
    dplyr::mutate(album.s.release_date = ifelse(stringr::str_length(.data[['release_date']]) == 4, paste0(.data[['release_date']], '-01-01'),.data[['release_date']])) %>%
    dplyr::mutate(album.s.release_date = .data[['album.s.release_date']] %>% as.Date()) %>%
    dplyr::select('album.s.id' = 'id',
           'album.s.type' = 'type',
           'album.s.upc' = 'external_ids.upc',
           'album.s.total_tracks' = 'total_tracks',
           'album.s.release_date',
           'album.s.release_year',
           'album.s.label' = 'label',
           'album.s.popularity' = 'popularity')
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


clean_artists <- function(artistsRaw) {
  artistsRaw %>%
    dplyr::select('artist.s.id' = 'id',
           'artist.s.genres' = 'genres',
           'artist.s.popularity' = 'popularity',
           'artist.s.followers' = 'followers.total') %>%
    tidyr::hoist('artist.s.genres', artist.s.topGenre = list('id', 1L), .remove = FALSE)
}

expand_artists <- function(trackframe){
  idflag <- FALSE
  if('id' %in% colnames(trackframe)) {
    trackframe <- dplyr::rename(trackframe, 'id_temp' = 'id')
    idflag <- TRUE
  }
  trackframe <- trackframe %>%
    tidyr::unnest(cols = 'track.s.artistlist') %>%
    dplyr::rename('artist.s.name' = 'name',
           'artist.s.id' = 'id',) %>%
    dplyr::select(-'href',-'type',-'uri',-'external_urls.spotify')
  if(idflag) {trackframe <- dplyr::rename(trackframe, 'id' = 'id_temp')}
  return(trackframe)
}
