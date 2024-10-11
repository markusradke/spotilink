#'Get \emph{Acousticbrainz} Track Information
#'
#'Retrieve a data frame containing information from the \emph{Acousticbrainz} API using the IDs from retrieved from Musicbrainz. Filtering information with \code{\link{filter_quality_musicbrainz_acousticbrainz_tracks}} will be based on the track and firstartist quality of the corresponding Musicbrainz entry.
#'The result contains information on tracks.
#'The \pkg{spotilink} naming convention is used.
#'
#' @param input
#'Data Frame containing the following columns:
#'\itemize{
#'  \item\code{track.mb.id} \cr
#'  with \emph{Musicbrainz} track ids,
#'  \item\code{track.mb.quality} \cr
#'  with \emph{Musicbrainz} track quality, calculated using the Jaro-Winkler distance (ranging from 0 to 1).
#'}
#'It is advisable to first run \code{\link{get_tracks_musicbrainz}} before running this command, in order to have all the necessary information.
#'
#'
#' @return Data Frame with added information from the \emph{Acousticbrainz} API using the  \pkg{spotilink} naming convention.
#' @export
#'
#'@examples
get_tracks_acousticbrainz <- function(input){
  are_needed_columns_present(input, c('track.mb.id', 'track.mb.quality'))
  input <- rename_existing_variables(input, c(acousticbrainzTrackVars))

  unique_mbids <- na.omit(unique(input$track.mb.id))

  message('Searching mbids on acousticbrainz...')
  if(file.exists('acousticbrainz_search.rds')){
    abids <- readRDS('acousticbrainz_search.rds')
  }
  else{
    message('Detected search results. Loading...')
    abids <- search_mbids_on_acousticbrainz(unique_mbids)
  }
  saveRDS(abids, 'acousticbrainz_search.rds')
  abids <<- abids

  message('Looking up tracks on acousticbrainz...')
  if(length(abids) == 0) {
    acousticbrainz_tracks <- purrr::map_df(input$track.s.id,
                                           make_na_frame_acousticbrainz_tracks)
  }
  else {
    acousticbrainz_tracks <- lookup_tracks_on_acousticbrainz(abids)
  }
  message('Done.')


  result <- suppressMessages(dplyr::left_join(input, acousticbrainz_tracks))
  print_linkage_for_id('track.ab.id', result)
  saveRDS(result, 'acousticbrainz.rds')
  result
}

search_mbids_on_acousticbrainz <- function(unique_mbids){
  abids <- c()
  start <- 1
  stepsize <- 25
  repeat{
    message(paste0('Searching track ', start, ' to ', stepsize - 1, ' of ', length(unique_mbids), '...'))
    end <- min(c(start+stepsize-1,length(unique_mbids)))
    url <- paste0('https://acousticbrainz.org/api/v1/count?recording_ids=',
                  paste0(unique_mbids[start:end], collapse = ';'))
    response <- get_api_with_connection_management(url)
    abids_temp <- gsub('.count','',response %>% unlist %>% names)
    abids <- c(abids, abids_temp)

    start <- start + stepsize
    if (start > length(unique_mbids)) {break}
  }
  message(paste0('Found ',length(abids),' of ',length(unique_mbids),' mbids on Acousticbrainz.'))
  abids
}

lookup_tracks_on_acousticbrainz <- function(abids){
  result <- c()
  start <- 1
  stepsize <- 25
  repeat{
    end <- min(c(start + stepsize - 1,length(abids)))
    message(paste0('Looking up track ', start, ' to ', end - 1, ' of ', length(abids), '...'))
    # manche haben nur lowlevel und keine highlevel features, bzw. vielleicht auch anders herum, das muss berücksichtigt werden, fehler bei 1351:(1351+25)
    url <- paste0('https://acousticbrainz.org/api/v1/high-level?recording_ids=',
                  paste0(abids[start:end], collapse=';'),
                  '&map_classes=true')
    response <- get_api_with_connection_management(url)
    result_highlevel <- response %>% unlist(recursive=F) %>% dplyr::as_tibble() %>% parse_tracks_acousticbrainz_highlevel()

    url <- paste0('https://acousticbrainz.org/api/v1/low-level?recording_ids=',
                  paste0(abids[start:end], collapse=';'),
                  '&map_classes=true')
    response <- get_api_with_connection_management(url)
    result_lowlevel <- response %>% unlist(recursive=F) %>% dplyr::as_tibble() %>% parse_tracks_acousticbrainz_lowlevel()


    result_temp <- suppressMessages(dplyr::left_join(result_highlevel, result_lowlevel))
    result <- rbind(result, result_temp)
    start <- start + stepsize
    if (start > length(abids)) {break}
  }
  result
}

parse_tracks_acousticbrainz_highlevel <- function(response){
  if(ncol(response) == 0){return(make_na_frame_acousticbrainz_tracks_highlevel('test'))}
  result <- c()
  for(i in 1:(ncol(response))){
    track <- response[i]
    metadata <- track[[1]]$metadata
    highlevel_features <- track[[1]]$highlevel

    mbid <- data.frame(track.mb.id = metadata$tags$musicbrainz_recordingid[[1]],
                       track.ab.id = metadata$tags$musicbrainz_recordingid[[1]])

    features  <- highlevel_features %>% sapply('[[','value')  %>% t %>% dplyr::as_tibble()
    features <- dplyr::rename_with(features, ~paste0('track.ab.', names(features))) %>%
      dplyr::rename_with(~ stringr::str_remove_all(., 'mood_|moods_|_'), everything())
    features <<- features


    genre_drtmd_p <- highlevel_features[['genre_dortmund']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    genre_drtmd_p <- genre_drtmd_p %>%  dplyr::rename_with(~paste0('track.ab.p.drtmd.',gsub(' ', '', tolower(names(genre_drtmd_p)))))

    genre_elctr_p <- highlevel_features[['genre_electronic']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    genre_elctr_p <- genre_elctr_p %>%  dplyr::rename_with(~paste0('track.ab.p.eltrc.',gsub(' ', '', tolower(names(genre_elctr_p)))))

    genre_rosa_p <- highlevel_features[['genre_rosamerica']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    genre_rosa_p <- genre_rosa_p %>%  dplyr::rename_with(~paste0('track.ab.p.rosa.',gsub(' ', '', tolower(names(genre_rosa_p)))))

    genre_tzntk_p <- highlevel_features[['genre_tzanetakis']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    genre_tzntk_p <- genre_tzntk_p %>%  dplyr::rename_with(~paste0('track.ab.p.tzntk.',gsub(' ', '', tolower(names(genre_tzntk_p)))))

    rhythm_ismir_p <- highlevel_features[['ismir04_rhythm']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    rhythm_ismir_p <- rhythm_ismir_p %>%  dplyr::rename_with(~paste0('track.ab.p.ismir.',gsub('-', '',gsub(' ', '', tolower(names(rhythm_ismir_p))))))

    moods_mirex_p <- highlevel_features[['moods_mirex']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    moods_mirex_p <- moods_mirex_p %>%  dplyr::rename_with(~paste0('track.ab.p.mirex.', stringr::str_extract(tolower(names(moods_mirex_p)), '^[^,]+')))

    result_temp <- data.frame(mbid,
                              features,
                              track.ab.p.danceable = highlevel_features$danceability$all$Danceable,
                              track.ab.p.female = highlevel_features$gender$all$female,
                              track.ab.p.acoustic = highlevel_features$mood_acoustic$all$Acoustic,
                              track.ab.p.aggressive = highlevel_features$mood_aggressive$all$Aggressive,
                              track.ab.p.electronic = highlevel_features$mood_electronic$all$Electronic,
                              track.ab.p.happy = highlevel_features$mood_happy$all$Happy,
                              track.ab.p.party = highlevel_features$mood_party$all$Party,
                              track.ab.p.relaxed = highlevel_features$mood_relaxed$all$Relaxed,
                              track.ab.p.sad = highlevel_features$mood_sad$all$Sad,
                              track.ab.p.bright = highlevel_features$timbre$all$bright,
                              track.ab.p.tonal = highlevel_features$tonal_atonal$all$tonal,
                              track.ab.p.voice = highlevel_features$voice_instrumental$all$voice,
                              genre_drtmd_p,
                              genre_elctr_p,
                              genre_rosa_p,
                              genre_tzntk_p,
                              rhythm_ismir_p,
                              moods_mirex_p)
    result <- rbind(result, result_temp)
  }
  result
}

parse_tracks_acousticbrainz_lowlevel <- function(response){
  response <<- response
  result <- c()
  for(i in 1:(ncol(response))){
    track <- response[i]
    metadata <- track[[1]]$metadata
    lowlevel_features <- track[[1]]$lowlevel
    rhythm_features <- track[[1]]$rhythm
    tonal_features <- track[[1]]$tonal

    track.ab.rhythm.tempo <- rhythm_features$bpm
    track.ab.rhythm.danceability <- rhythm_features$danceability
    track.ab.rhythm.onsetrate <- rhythm_features$onset_rate
    track.ab.low.loudness <- lowlevel_features$average_loudness
    track.ab.low.dynamiccomplexity <- lowlevel_features$dynamic_complexity
    track.ab.tonal.chordchangerate <- tonal_features$chords_changes_rate
    track.ab.tonal.key <- tonal_features$key_key
    track.ab.tonal.chordsnumberrate <- tonal_features$chords_number_rate
    track.ab.tonal.mode <- tonal_features$key_scale
    track.ab.tonal.keystrength <- tonal_features$key_strength

    result_temp <- data.frame(track.mb.id = metadata$tags$musicbrainz_recordingid[[1]],
                              track.ab.id = metadata$tags$musicbrainz_recordingid[[1]],
                              ifelse(!is.null(track.ab.rhythm.tempo), track.ab.rhythm.tempo, NA),
                              ifelse(!is.null(track.ab.rhythm.danceability),track.ab.rhythm.danceability, NA),
                              ifelse(!is.null(track.ab.rhythm.onsetrate), track.ab.rhythm.onsetrate, NA),
                              ifelse(!is.null(track.ab.low.loudness), track.ab.low.loudness, NA),
                              ifelse(!is.null(track.ab.low.dynamiccomplexity), track.ab.low.dynamiccomplexity, NA),
                              ifelse(!is.null(track.ab.tonal.chordchangerate), track.ab.tonal.chordchangerate, NA),
                              ifelse(!is.null(track.ab.tonal.key), track.ab.tonal.key, NA),
                              ifelse(!is.null(track.ab.tonal.chordsnumberrate), track.ab.tonal.chordsnumberrate, NA),
                              ifelse(!is.null(track.ab.tonal.mode), track.ab.tonal.mode, NA),
                              ifelse(!is.null(track.ab.tonal.keystrength), track.ab.tonal.keystrength, NA))
    result <- rbind(result, result_temp)
  }
  result
}
