#'Get \emph{Acousticbrainz} Track Information
#'
#'Retrieve a data frame containing information from the \emph{Acousticbrainz} API using the IDs from retrieved from Musicbrainz.
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
  abids <- search_mbids_on_acousticbrainz(unique_mbids)
  message('Looking up tracks on acousticbrainz...')
  acousticbrainz_tracks <- lookup_tracks_on_acousticbrainz(abids)
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
    url <- paste0('https://acousticbrainz.org/api/v1/high-level?recording_ids=',
                  paste0(abids[start:end], collapse=';'),
                  '&map_classes=true')
    response <- get_api_with_connection_management(url)
    result_temp <- response %>% unlist(recursive=F) %>% dplyr::as_tibble() %>% parse_tracks_acousticbrainz()
    result <- rbind(result, result_temp)
    start <- start + stepsize
    if (start > length(abids)) {break}
  }
  result
}

parse_tracks_acousticbrainz <- function(response){
  result <- c()
  for(i in 1:(ncol(response))){
    track <- response[i]
    metadata <- track[[1]]$metadata
    highlevel_features <- track[[1]]$highlevel

    mbid <- data.frame(track.mb.id = metadata$tags$musicbrainz_recordingid[[1]],
                       track.ab.id = metadata$tags$musicbrainz_recordingid[[1]])
    features  <- highlevel_features %>% sapply('[[','value')   %>% t %>% dplyr::as_tibble()
    features <- features %>% dplyr::rename_with(~paste0('track.ab.', names(features)))
    probabilities <- highlevel_features %>% sapply('[[','probability') %>% t %>% dplyr::as_tibble()
    probabilities <- probabilities %>% dplyr::rename_with(~paste0('track.ab.p.', names(probabilities)))
    features_with_probabilities <- cbind(mbid,features,probabilities)[order(c(seq_along(features), seq_along(probabilities)))] %>%
      dplyr::as_tibble()

    danceability_p <- highlevel_features[['danceability']][['all']] %>% unlist %>% t %>%dplyr::as_tibble()
    danceability_p <- danceability_p %>%  dplyr::rename_with(~paste0('track.ab.p.',gsub(' ', '', tolower(names(danceability_p)))))

    gender_p <- highlevel_features[['gender']][['all']] %>% unlist %>% t %>%dplyr::as_tibble()
    gender_p <- gender_p %>%  dplyr::rename_with(~paste0('track.ab.p.', names(gender_p)))

    genre_p <- highlevel_features[['genre_rosamerica']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    genre_p <- genre_p %>%  dplyr::rename_with(~paste0('track.ab.p.rosa.',gsub(' ', '', tolower(names(genre_p)))))

    mood_acoustic_p <- highlevel_features[['mood_acoustic']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    mood_acoustic_p <- mood_acoustic_p %>%  dplyr::rename_with(~paste0('track.ab.p.',gsub(' ', '', tolower(names(mood_acoustic_p)))))

    mood_aggressive_p <- highlevel_features[['mood_aggressive']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    mood_aggressive_p <- mood_aggressive_p %>%  dplyr::rename_with(~paste0('track.ab.p.',gsub(' ', '_', tolower(names(mood_aggressive_p)))))

    mood_electronic_p <- highlevel_features[['mood_electronic']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    mood_electronic_p <- mood_electronic_p %>%  dplyr::rename_with(~paste0('track.ab.p.',gsub(' ', '', tolower(names(mood_electronic_p)))))

    mood_happy_p <- highlevel_features[['mood_happy']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    mood_happy_p <- mood_happy_p %>%  dplyr::rename_with(~paste0('track.ab.p.',gsub(' ', '', tolower(names(mood_happy_p)))))

    mood_party_p <- highlevel_features[['mood_party']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    mood_party_p <- mood_party_p %>%  dplyr::rename_with(~paste0('track.ab.p.',gsub(' ', '', tolower(names(mood_party_p)))))

    mood_relaxed_p <- highlevel_features[['mood_relaxed']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    mood_relaxed_p <- mood_relaxed_p %>%  dplyr::rename_with(~paste0('track.ab.p.',gsub(' ', '', tolower(names(mood_relaxed_p)))))

    mood_sad_p <- highlevel_features[['mood_sad']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    mood_sad_p <- mood_sad_p %>%  dplyr::rename_with(~paste0('track.ab.p.',gsub(' ', '', tolower(names(mood_sad_p)))))

    timbre_p <- highlevel_features[['timbre']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    timbre_p <- timbre_p %>%  dplyr::rename_with(~paste0('track.ab.p.', names(timbre_p)))

    tonal_atonal_p <- highlevel_features[['tonal_atonal']][['all']] %>% unlist %>% t %>%dplyr::as_tibble()
    tonal_atonal_p <- tonal_atonal_p %>%  dplyr::rename_with(~paste0('track.ab.p.',names(tonal_atonal_p)))

    voice_instrumental_p <- highlevel_features[['voice_instrumental']][['all']] %>% unlist %>% t %>% dplyr::as_tibble()
    voice_instrumental_p <- voice_instrumental_p %>%  dplyr::rename_with(~paste0('track.ab.p.',names(voice_instrumental_p)))


    result_temp <- cbind(features_with_probabilities,
                             danceability_p,
                             gender_p,
                             genre_p,
                             mood_acoustic_p,
                             mood_aggressive_p,
                             mood_electronic_p,
                             mood_happy_p,
                             mood_party_p,
                             mood_relaxed_p,
                             mood_sad_p,
                             timbre_p,
                             tonal_atonal_p,
                             voice_instrumental_p)
    result <- rbind(result, result_temp)
  }
    result
}
