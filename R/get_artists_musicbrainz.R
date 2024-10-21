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
  artists <- input %>% dplyr::filter(! is.na(artist.s.id)) %>% dplyr::distinct(artist.s.id, .keep_all = TRUE)

  if (!('track.mb.artistlist' %in% colnames(artists) & 'track.mb.quality' %in% colnames(artists))) { # add NA cols for artistlist and track quality for searching for artists only
    artists <- artists %>%
      dplyr::mutate('track.mb.artistlist' = list(NA)) %>%
      dplyr::mutate('track.mb.quality' = NA)
  }

  artists <- dplyr::select(artists, 'artist.s.id', 'artist.s.name', 'track.mb.artistlist','track.mb.quality')

  artist_mbids <- search_artist_mbids(artists)
  result <- lookup_artists_mb(artist_mbids)


  n_found_by_id <- result %>% dplyr::filter(artist.mb.foundbyid) %>% nrow()
  message(paste0(round(n_found_by_id / nrow(artists) * 100, 2), '% distinct artists were found using the Musicbrainz ID from the track information.'))
  result <- result %>% filter_quality_musicbrainz_artists(artist_threshold) %>%
    dplyr::mutate(artist.mb.birth = as.Date(.data[['artist.mb.birth']]))  %>% # Date conversion must be here; else only integers are returned
    dplyr::mutate(artist.mb.death = as.Date(.data[['artist.mb.death']]))
  result <- dplyr::left_join(input, result, by = c('artist.s.id'))

  saveRDS(result, 'mb_artists.rds')
  mbartist_remove_checkpoints()
  result
}


search_artist_mbids <- function(artists){
  checkpoint_name <- 'mb_artists_search'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  if(! last_index == nrow(artists)){
    saved_data <- checkpoint$saved_data
    if(last_index > 0) {artists <- tail(artists, -last_index)}
    purrr::pmap_df(artists,
                   search_single_artist_mbid %>% save_checkpoint_and_count(checkpoint_name,
                                                                           last_index,
                                                                           saved_data,
                                                                           savingstep = 20,
                                                                           ndatapoints = nrow(artists)),
                   .progress = 'Searching artists on Musicbrainz...')
  }
  else{message('Artist search already done.')}
  suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
}

search_single_artist_mbid <- function(artist.s.id, artist.s.name, track.mb.artistlist, track.mb.quality) {
  if(!is.na(track.mb.quality)) {
    artist<- track.mb.artistlist %>%
      dplyr::select('artist_mbid', 'name') %>%
      dplyr::filter(.data[['name']] %>% simplify_name() == artist.s.name %>% simplify_name())
    artist.mb.id <- artist$artist_mbid[1]
  }
  else {
    artist.mb.id <- NA
  }
  if(! is.na(artist.mb.id)){
    artist.mb.foundbyid <- TRUE
    artist.mb.quality <- stringdist::stringsim(artist.s.name %>% simplify_name(),
                                               artist$name %>% simplify_name(),
                                               'jw')
  }
  else {
    artist.mb.foundbyid <- FALSE
    artist <- suppressMessages(musicbrainz::search_artists(artist.s.name)[1,])
    if(! all(is.na(artist))) {
      artist.mb.quality <-  stringdist::stringsim(artist.s.name %>% simplify_name(),
                                                  artist$name %>% simplify_name(),
                                                  'jw')
      artist.mb.id <- artist$mbid
    }
    else {
      artist.mb.quality <- NA
      artist.mb.id <- NA
    }
  }
  dplyr::tibble(artist.s.id = artist.s.id,
                artist.mb.id = artist.mb.id,
                artist.mb.quality = artist.mb.quality,
                artist.mb.foundbyid = artist.mb.foundbyid)
}


lookup_artists_mb <- function(artist_mbids){
  checkpoint_name <- 'mb_artists_lookup'
  checkpoint <- read_checkpoint(checkpoint_name)
  last_index <- checkpoint$last_index
  if(! last_index == nrow(artist_mbids)){
    saved_data <- checkpoint$saved_data
    if(last_index > 0) {artist_mbids <- tail(artist_mbids, -last_index)}
    purrr::pmap_df(artist_mbids,
                   lookup_single_artist_mb %>% save_checkpoint_and_count(checkpoint_name,
                                                                         last_index,
                                                                         saved_data,
                                                                         savingstep = 20,
                                                                         ndatapoints = nrow(artist_mbids)),
                   .progress = 'Looking up artist information on Musicbrainz...')
  }
  else{message('Artist lookup already done.')}
  suppressMessages(read_checkpoint(checkpoint_name)$saved_data)
}

lookup_single_artist_mb <- function(artist.s.id, artist.mb.id, artist.mb.quality, artist.mb.foundbyid) {
  if(is.na(artist.mb.id)){return(make_na_frame_musicbrainz_artists(artist.s.id = artist.s.id))}

  musicbrainz::lookup_artist_by_id(artist.mb.id, includes = c('tags')) %>%
    dplyr::mutate(artist.mb.birth = ifelse(stringr::str_length(.data[['life_span_begin']]) == 4, paste0(.data[['life_span_begin']], '-01-01'),.data[['life_span_begin']])) %>%
    dplyr::mutate(artist.mb.birthyear = stringr::str_sub(.data[['life_span_begin']], 1,4) %>% as.integer()) %>%
    dplyr::mutate(artist.mb.death = ifelse(stringr::str_length(.data[['life_span_end']]) == 4,
                                           paste0(.data[['life_span_end']], '-01-01'),
                                           .data[['life_span_end']]),
                  artist.mb.death = ifelse(stringr::str_length(artist.mb.death) == 7,
                                           paste0(artist.mb.death, '-01'),
                                           artist.mb.death)) %>%
    dplyr::mutate(artist.mb.deathyear = stringr::str_sub(.data[['life_span_end']], 1,4) %>% as.integer()) %>%
    dplyr::mutate(artist.mb.dead = .data[['life_span_ended']] %>% as.logical()) %>%
    cbind(artist.s.id, artist.mb.id, artist.mb.quality, artist.mb.foundbyid) %>%
    dplyr::select('artist.s.id',
                  'artist.mb.name' = 'name',
                  'artist.mb.id',
                  'artist.mb.quality',
                  'artist.mb.foundbyid',
                  'artist.mb.type' = 'type',
                  'artist.mb.gender' = 'gender',
                  'artist.mb.area' = 'country',
                  'artist.mb.birth',
                  'artist.mb.birthyear',
                  'artist.mb.death',
                  'artist.mb.deathyear',
                  'artist.mb.dead',
                  'tags',
                  'artist.mb.origin' = 'begin_area_name') %>%
  retrieve_artist_genre()
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


mbartist_remove_checkpoints <- function(){
  pattern <- paste0('^mb_artists_(search|lookup)_(\\d+)\\.rds$')
  files <- list.files()
  matching_files <- grep(pattern, files, value = TRUE)
  for(file in matching_files){file.remove(file)}
}
