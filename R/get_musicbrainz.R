#'Get \emph{MusicBrainz} Information
#'
#'Retrieve a data frame containing information from the \emph{MusicBrainz} API.
#'The result contains information on the tracks as well as corresponding albums and artists.
#'The \pkg{spotilink} naming convention is used.
#'
#' @param charts
#'Data Frame containing the following columns:
#'\itemize{
#'  \item\code{track.s.id} \cr
#'  with \emph{Spotify} track ids,
#'  \item \code{track.s.title} \cr
#'  with \emph{Spotify} track name,
#'  \item \code{track.s.isrc} \cr
#'  with track ISRCs from \emph{Spotify},
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
#'It is advisable to first run \code{\link{get_spotify}} before running this command,
#'in order to have all the necessary information.
#'
#' @return Data Frame with added information from the \emph{MusicBrainz} API using the  \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
get_musicbrainz <- function(charts) {
  charts <- pull_tracks(charts)
  charts <- pull_albums(charts)
  charts <- pull_artists(charts)
  charts <- charts %>%
    filter_low_quality() %>%
    combine_genres()
  print_linking_success(charts, c('track.s.id', 'album.s.id', 'artist.s.id'))
  charts
}

pull_tracks <- function(distinctCharts) {
  distinctCharts %>%
    dplyr::distinct(.data[['track.s.id']], .keep_all = TRUE) %>%
    retrieve_tracks() %>%
    retrieve_track_genre() %>%
    dplyr::left_join(distinctCharts, .data, by = c('track.s.id'))
}

retrieve_tracks <- function(distinctCharts){
  mbTracks <- c()
  isrcCounter <<- 0L
  n <- nrow(distinctCharts)
  cat('---------------------------------------------------\n')
  cat('Looking for tracks in Musicbrainz...\n')
  for (i in 1:nrow(distinctCharts)){
    cat('---------------------------------------------------\n')
    cat('track', i, 'of', n, '\n')
    result <- find_tracks_with_ISRC(distinctCharts[i,])
    if  (nrow(result) == 0) {
      result <- find_tracks_without_ISRC(distinctCharts[i,])
    }
    mbTracks <- rbind(mbTracks, result)
  }
  cat('---------------------------------------------------\n')
  cat(paste0(round(isrcCounter / nrow(distinctCharts),4) * 100, '% of tracks were found using the ISRC.\n'))
  rm(isrcCounter, pos = .GlobalEnv)
  mbTracks <- mbTracks %>%
    dplyr::select('track.mb.id' = 'mbid',
           'track.mb.title' = 'title',
           'track.mb.quality',
           'track.mb.artistlist' = 'artists',
           'track.mb.releases' = 'releases')
  cbind(track.s.id = distinctCharts$track.s.id, mbTracks)
}

find_tracks_with_ISRC <- function(observation) {
  result <- musicbrainz::search_recordings(paste0('isrc:', observation$track.s.isrc))
  if (nrow(result) != 0){
    cat('found via ISRC\n')
    isrcCounter <<- isrcCounter + 1
    result <- result[1,] %>%
      dplyr::mutate(track.mb.quality = 1)
  }
  result
}

find_tracks_without_ISRC <- function(observation) {
  cat('no ISRC, searching...\n')
  musicbrainz::search_recordings(paste0('artist:', observation$artist.s.name,' and recording:', observation$track.s.title)) %>%
    .data[1,] %>%
    dplyr::mutate(track.mb.quality = calculate_and_print_quality(search = observation$track.s.title,
                                                       found = .data[['title']]))
}

calculate_and_print_quality <- function(search, found) {
  search <- search %>% simplify_name()
  found <- found %>% simplify_name()
  quality <- stringdist::stringsim(found, search, 'jw')
  cat('search:', search, '\n')
  cat('found:', found, '\n')
  cat('quality:', quality,'\n')
  quality
}

simplify_name <- function(trackname) {
  trackname %>%
    stringr::str_replace(pattern = ' \\(.*', replacement = '') %>%
    stringr::str_replace(pattern = ' -.*', replacement = '') %>%
    toupper()
}

retrieve_track_genre <- function(tracks){
  trackGenres <- purrr::map_df(tracks$track.mb.id, lookup_musibrainz_track_tags_from_ID, .progress = TRUE) %>%
    get_highest_ranking_genre() %>%
    dplyr::rename('track.mb.genres' = 'genres',
           'track.mb.topGenre' = 'topGenre') %>%
    dplyr::mutate(track.mb.topGenre = .data[['track.mb.topGenre']] %>% as.character())
  cbind(tracks, trackGenres)
}

lookup_musibrainz_track_tags_from_ID  <- function(mbID) {
  musicbrainz::lookup_recording_by_id(mbID, includes=c('tags')) %>%
    dplyr::mutate(score = .data[['score']] %>% as.character()) %>%
    dplyr::mutate(length = .data[['length']] %>% as.integer())
}

pull_albums <- function(distinctCharts) {
  distinctCharts %>%
    dplyr::distinct(.data[['album.s.id']], .keep_all = TRUE) %>%
    retrieve_albums() %>%
    retrieve_album_genres() %>%
    dplyr::left_join(distinctCharts, ., by = c('album.s.id'))
}

retrieve_albums <- function(distinctCharts){
  mbAlbums <- c()
  upcCounter <<- 0L
  n <- nrow(distinctCharts)
  cat('---------------------------------------------------\n')
  cat('Looking for albums in Musicbrainz...\n')
  for (i in 1:nrow(distinctCharts)){
    cat('---------------------------------------------------\n')
    cat('album', i, 'of', n, '\n')
    result <- find_album_with_UPC(distinctCharts[i,])
    if  (nrow(result) == 0) {
      result <- find_album_without_UPC(distinctCharts[i,])
    }
    mbAlbums <- rbind(mbAlbums, result)
  }
  cat('---------------------------------------------------\n')
  cat(paste0(round(upcCounter / nrow(distinctCharts),4) * 100, '% of albums were found using the UPC.\n'))
  rm(upcCounter, pos = .GlobalEnv)
  mbAlbums <- mbAlbums %>%
    dplyr::select('album.mb.id' = 'mbid',
           'album.mb.title' = 'title',
           'album.mb.quality')
  cbind(album.s.id = distinctCharts$album.s.id, mbAlbums)
}

find_album_with_UPC <- function(observation) {
  result <- musicbrainz::search_releases(paste0('barcode:', observation$album.s.upc))
  if (nrow(result) != 0){
    cat('found via UPC\n')
    upcCounter <<- upcCounter + 1
    result <- result[1,] %>%
      dplyr::mutate(album.mb.quality = 1)
  }
  result
}

find_album_without_UPC <- function(observation) {
  cat('no UPC, searching...\n')
  musicbrainz::search_releases(paste0('artist:', observation$artist.s.name,' and release:', observation$album.s.title)) %>%
    .data[1,] %>%
    dplyr::mutate(album.mb.quality = calculate_and_print_quality(search = observation$album.s.title,
                                                       found = .data[['title']]))
}

retrieve_album_genres <- function(albums){
  albumGenres <- purrr::map_df(albums$album.mb.id, lookup_musicbrainz_album_tags_from_ID, .progress = TRUE) %>%
    get_highest_ranking_genre() %>%
    dplyr::rename('album.mb.genres' = 'genres',
           'album.mb.topGenre' = 'topGenre') %>%
    dplyr::mutate(album.mb.topGenre = .data[['album.mb.topGenre']] %>% as.character())
  cbind(albums, albumGenres)
}


lookup_musicbrainz_album_tags_from_ID  <- function(mbID) {
  musicbrainz::lookup_release_by_id(mbID, includes=c('tags')) %>%
    dplyr::mutate(score = .data[['score']] %>% as.character())
}


pull_artists <- function(distinctCharts) {
  cat('---------------------------------------------------\n')
  cat('Looking for artists in Musicbrainz...\n')
  artistMBIDCounter <<- 0
  artists <- distinctCharts %>%
    dplyr::distinct(.data[['artist.s.id']], .keep_all = TRUE)
  noOfArtists <- nrow(artists)
  artists <- artists %>%
    dplyr::select('artist.s.id',
                  'artist.s.name',
                  'track.mb.artistlist',
                  'track.mb.quality') %>%
    purrr::pmap_df(get_artist_MBIDs %>% count_artists(noOfArtists), .progress = TRUE) %>%
    purrr::pmap_df(lookup_artists %>% count_artists(noOfArtists), .progress = TRUE) %>%
    retrieve_artist_genre() %>%
    dplyr::mutate(artist.mb.birth = as.Date(.data[['artist.mb.birth']]))  %>%
    dplyr::mutate(artist.mb.death = as.Date(.data[['artist.mb.death']])) %>%
    dplyr::left_join(distinctCharts, ., by = c('artist.s.id'))
  cat('---------------------------------------------------\n')
  cat(paste0(artistMBIDCounter / nrow(distinctCharts) * 100, '% were found using the MBID from the track information. \n'))
  rm(artistMBIDCounter, pos = .GlobalEnv)
  artists
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
  artist<- track.mb.artistlist %>%
    dplyr::select('artist_mbid', 'name') %>%
    dplyr::filter(.data[['name']] == artist.s.name)
  artist.mb.id <- artist$artist_mbid[1]
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
  artistGenres <- artists %>%
    get_highest_ranking_genre() %>%
    dplyr::rename('artist.mb.genres' = 'genres',
           'artist.mb.topGenre' = 'topGenre')
  cbind(artists, artistGenres) %>%
    dplyr::select(-'tags') %>%
    dplyr::mutate(artist.mb.topGenre = .data[['artist.mb.topGenre']] %>% as.character())
}

get_highest_ranking_genre <- function(tagLookup) {
  highest_tag <- tagLookup$tags %>%
    purrr::lmap(unzip_tags) %>%
    unlist() %>%
    dplyr::tibble(topGenre = .)
  genresMB <- cbind(tagLookup, highest_tag) %>%
    dplyr::select('genres' = 'tags', 'topGenre')
}

unzip_tags <- function(tags) {
  tags <- tags[[1]]
  if (ncol(tags == 2)) {
    tags <- dplyr::filter(tags, .data[['tag_name']] %in% musicbrainzWhitelist$genres)
    tags <- dplyr::arrange(tags, dplyr::desc('tag_count'))
    return(
      list(tags[['tag_name']][1])
    )
  }
  else {
    return(list(NA))
  }
}

combine_genres <- function(charts) {
  charts %>%
    dplyr::mutate(track.mb.combinedGenre = ifelse(!is.na(.data[['track.mb.topGenre']]), .data[['track.mb.topGenre']], .data[['album.mb.topGenre']])) %>%
    dplyr::mutate(track.mb.combinedGenre = ifelse(!is.na(.data[['track.mb.combinedGenre']]), .data[['track.mb.combinedGenre']], .data[['artist.mb.topGenre']])) %>%
    dplyr::mutate(album.mb.combinedGenre = ifelse(!is.na(.data[['album.mb.topGenre']]), .data[['album.mb.topGenre']], .data[['track.mb.topGenre']])) %>%
    dplyr::mutate(album.mb.combinedGenre = ifelse(!is.na(.data[['album.mb.combinedGenre']]), .data[['album.mb.combinedGenre']], .data[['artist.mb.topGenre']])) %>%
    dplyr::mutate(artist.mb.combinedGenre = ifelse(!is.na(.data[['artist.mb.topGenre']]), .data[['artist.mb.topGenre']], .data[['album.mb.topGenre']])) %>%
    dplyr::mutate(artist.mb.combinedGenre = ifelse(!is.na(.data[['artist.mb.combinedGenre']]), .data[['artist.mb.combinedGenre']], .data[['track.mb.topGenre']]))
}

filter_low_quality <- function(charts){
  cat('---------------------------------------------------\n')
  cat('Filter Data with Quality < 0.8 ... \n')

  charts %>%
    dplyr::mutate(dplyr::across(dplyr::contains('track.mb'), ~ ifelse(.data[['track.mb.quality']] >= 0.8, .x, NA))) %>%
    dplyr::mutate(dplyr::across(dplyr::contains('album.mb'), ~ ifelse(.data[['album.mb.quality']] >= 0.8, .x, NA))) %>%
    dplyr::mutate(dplyr::across(dplyr::contains('artist.mb'), ~ ifelse(.data[['artist.mb.quality']] >= 0.8, .x, NA)))

}

print_linking_success <- function(charts, sIDcols) {
  for (sIDcol in sIDcols) {
    mbidCol <- stringr::str_replace(sIDcol, '\\.s\\.', '\\.mb\\.')
    distinctCharts <- charts %>%
      dplyr::distinct(.data[[sIDcol]], .keep_all = TRUE)
    ratioFound <- nrow(distinctCharts %>% tidyr::drop_na(dplyr::all_of(mbidCol))) / nrow(distinctCharts)
    cat(paste0('Found ', round(ratioFound,4) * 100, '% of distinct ', sIDcol, ' on Musicbrainz.'), '\n')
  }
}
