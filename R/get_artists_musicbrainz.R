#'Get \emph{MusicBrainz} Artist Information
#'
#'Retrieve a data frame containing information from the \emph{MusicBrainz} API.
#'The result contains information on artists.
#'The \pkg{spotilink} naming convention is used.
#'
#' @param input
#'Data Frame containing the following columns:
#'\itemize{
#'  \item \code{artist.s.id} \cr
#'  with \emph{Spotify} artist id,
#'  \item \code{artist.s.name} \cr
#'  with \emph{Spotify} artist name.
#'}
#'It is advisable to first run \code{\link{get_artists_spotify}} before running this command,
#'in order to have all the necessary information.
#'
#'@param artist_threshold
#'Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
#'The values correspond to the string similarity (1 - Jaro-Winkler distance) between the artist name on \emph{Spotify} and the found artist name on \emph{Musicbrainz}.
#'
#' @return Data Frame with added information from the \emph{MusicBrainz} API using the  \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
#'data <- data.frame(artist.s.id = c('5aIqB5nVVvmFsvSdExz408'),
#'                   artist.s.name = c('Johann Sebastian Bach'))
#'
#'get_artists_musicbrainz(data)
get_artists_musicbrainz <- function(input, artist_threshold = 0.8) {
  are_needed_columns_present(input, c('artist.s.id', 'artist.s.name'))
  renameVars <- musicbrainzArtistVars[! musicbrainzArtistVars %in% c('artist.s.id', 'artist.s.name')]
  res <- rename_existing_variables(input, renameVars)

  pull_artists_musicbrainz(res, artist_threshold)
}

pull_artists_musicbrainz <- function(input, artist_threshold) {
  cat('---------------------------------------------------\n')
  cat('Looking for artists in Musicbrainz...\n')
  artistMBIDCounter <<- 0
  artists <- input %>%
    dplyr::distinct(.data[['artist.s.id']], .keep_all = TRUE)
  noOfArtists <- nrow(artists)
  # add NA cols for artistlist and track quality for searching for artists only
  if (!('track.mb.artistlist' %in% colnames(artists) & 'track.mb.quality' %in% colnames(artists))) {
    artists <- artists %>%
      dplyr::mutate('track.mb.artistlist' = list(NA)) %>%
      dplyr::mutate('track.mb.quality' = NA)
  }

  artists <- artists %>%
    dplyr::select('artist.s.id',
                  'artist.s.name',
                  'track.mb.artistlist',
                  'track.mb.quality') %>%
    purrr::pmap_df(get_artist_MBIDs %>% count_artists(noOfArtists), .progress = TRUE) %>%
    purrr::pmap_df(lookup_artists %>% count_artists(noOfArtists), .progress = TRUE) %>%
    retrieve_artist_genre() %>%
    dplyr::left_join(input, ., by = c('artist.s.id'))
  cat('---------------------------------------------------\n')
  cat(paste0(round(artistMBIDCounter / nrow(input) * 100, 2), '% were found using the MBID from the track information. \n'))
  rm(artistMBIDCounter, pos = .GlobalEnv)
  artists %>%
    filter_lowquality_content('artist.mb.quality', artist_threshold, musicbrainzArtistVars) %>%
    dplyr::mutate(artist.mb.birth = as.Date(.data[['artist.mb.birth']]))  %>% # Date conversion must be here; else only integers are returned
    dplyr::mutate(artist.mb.death = as.Date(.data[['artist.mb.death']]))
}


count_artists <- function(f, noOfArtists){
  force(f)
  force(noOfArtists)

  i <- 0
  function(...) {
    i <<- i + 1
    cat('---------------------------------------------------\n')
    cat(paste0('Artist ', i, ' of ', noOfArtists, '\n'))
    f(...)
  }
}

get_artist_MBIDs <- function(artist.s.id, artist.s.name, track.mb.artistlist, track.mb.quality) {
  if(!is.na(track.mb.quality)) {
    artist<- track.mb.artistlist %>%
      dplyr::select('artist_mbid', 'name') %>%
      dplyr::filter(.data[['name']] == artist.s.name)
    artist.mb.id <- artist$artist_mbid[1]
  }
  else {
    artist.mb.id <- NA
  }
  if(! is.na(artist.mb.id)){
    artistMBIDCounter <<- artistMBIDCounter + 1
    cat('found via mbid \n')
    artist.mb.quality <- track.mb.quality
  }
  else {
    artist <- musicbrainz::search_artists(artist.s.name)[1,]
    if(! all(is.na(artist))) {
      artist.mb.quality <- calculate_and_print_quality(search = artist.s.name,
                                                       found = artist$name)
      artist.mb.id <- artist$mbid
    }
    else {
      artist.mb.quality <- NA
      artist.mb.id <- NA
    }
  }
  cbind(artist.s.id, artist.mb.id, artist.mb.quality) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(artist.mb.quality = .data[['artist.mb.quality']] %>% as.double())
}


lookup_artists <- function(artist.s.id, artist.mb.id, artist.mb.quality) {
  cat('Looking up information... \n')
  musicbrainz::lookup_artist_by_id(artist.mb.id, includes = c('tags')) %>%
    dplyr::mutate(artist.mb.birth = ifelse(stringr::str_length(.data[['life_span_begin']]) == 4, paste0(.data[['life_span_begin']], '-01-01'),.data[['life_span_begin']])) %>%
    dplyr::mutate(artist.mb.birthyear = stringr::str_sub(.data[['life_span_begin']], 1,4) %>% as.integer()) %>%
    dplyr::mutate(artist.mb.death = ifelse(stringr::str_length(.data[['life_span_end']]) == 4, paste0(.data[['life_span_end']], '-01-01'),.data[['life_span_end']])) %>%
    dplyr::mutate(artist.mb.deathyear = stringr::str_sub(.data[['life_span_end']], 1,4) %>% as.integer()) %>%
    dplyr::mutate(artist.mb.dead = .data[['life_span_ended']] %>% as.logical()) %>%
    cbind(artist.s.id, artist.mb.id, artist.mb.quality) %>%
    dplyr::select('artist.s.id',
                  'artist.mb.name' = 'name',
                  'artist.mb.id',
                  'artist.mb.quality',
                  'artist.mb.type' = 'type',
                  'artist.mb.gender' = 'gender',
                  'artist.mb.area' = 'country',
                  'artist.mb.birth',
                  'artist.mb.birthyear',
                  'artist.mb.death',
                  'artist.mb.deathyear',
                  'artist.mb.dead',
                  'tags',
                  'artist.mb.origin' = 'begin_area_name')
}

retrieve_artist_genre <- function(artists){
  artistgenres <- artists %>%
    get_highest_ranking_genre() %>%
    dplyr::rename('artist.mb.genres' = 'genres',
                  'artist.mb.topgenre' = 'topgenre')
  cbind(artists, artistgenres) %>%
    dplyr::select(-'tags') %>%
    dplyr::mutate(artist.mb.topgenre = .data[['artist.mb.topgenre']] %>% as.character())
}
