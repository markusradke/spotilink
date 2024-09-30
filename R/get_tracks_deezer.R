get_tracks_deezer <- function(input, threshold = 0.8){
  are_needed_columns_present(input, c('track.s.id', 'track.s.title', 'track.s.firstartist.name', 'album.s.title'))
  input <- rename_existing_variables(input, geniusLyricsVars)

  input_distinct <- dplyr::distinct(input, track.s.id, track.s.firstartist.name, album.s.title, .keep_all = T)
  if('album.s.title' %in% colnames(input)){
    deezer_tracks <- purrr::pmap_df(list(input_distinct$track.s.title,
                                  input_distinct$track.s.firstartist.name,
                                  input_distinct$track.s.id,
                                  input_distinct$album.s.title),
                                  get_single_track_deezer, .progress = 'Retrieving tracks from Deezer...')
  }

  message('Done.')
  result <- suppressMessages(dplyr::left_join(input, deezer_tracks))
  print_linkage_for_id(result, 'track.dz.id')
  result
}

get_single_track_deezer <- function(track.s.title, artist.s.name, track.s.id, album.s.title){
  .make_empty_frame <- function(){
    data.frame(track.s.id = track.s.id,
               track.dz.id = NA,
               track.dz.title = NA,
               track.dz.quality = NA,
               track.dz.isrc = NA,
               track.dz.durationms = NA,
               track.dz.rank = NA,
               track.dz.explicit = NA,
               track.dz.explicitinfo = NA,
               track.dz.tempo = NA,
               track.dz.loudness = NA,
               artist.dz.id = NA,
               artist.dz.name = NA,
               artist.dz.quality = NA,
               album.dz.id = NA,
               album.dz.title = NA,
               album.dz.quality = NA)
  }

  .create_search_url <- function(track.s.title, artist.s.name, album.s.title = NA){
    paste0('https://api.deezer.com/search?q=artist:"', artist.s.name %>% simplify_name(),
          '" track:"',track.s.title %>% simplify_name(),
          '" album:"', album.s.title %>% simplify_name(), '"') %>% utils::URLencode()
  }

  .create_lookup_url <- function(track.dz.id){
    paste0('https://api.deezer.com/track/', track.dz.id)
  }

  .connection_management <- function(url){
    repeat {
      response <- httr::GET(url)
      if (httr::status_code(response) == 200) {
        res <- httr::content(response)
        return(res)
      } else if (httr::status_code(response) == 429) {
        message('Rate limit exceeded. Waiting for 45 seconds, then trying again...')
        Sys.sleep(45)
      } else {
        message('An error occurred: ', httr::status_code(response), ' - ', httr::content(response, 'text', encoding = 'UTF-8'))
        return(NULL)
      }
    }
  }

  .get_parsed_topresult <- function(result){
    topresults <- data.frame(track.dz.id = sapply(result$data, function(hit) hit$id),
                             track.dz.title = sapply(result$data, function(hit) hit$title),
                             artist.dz.id = sapply(result$data, function(hit) hit$artist$id %>% as.character()),
                             artist.dz.name = sapply(result$data, function(hit) hit$artist$name),
                             album.dz.id = sapply(result$data, function(hit) hit$album$id %>% as.character()),
                             album.dz.title = sapply(result$data, function(hit) hit$album$title))
    if(nrow(topresults) == 0){return(NULL)}
    topresults %>%
      dplyr::mutate(track.dz.quality = stringdist::stringsim(track.s.title %>% simplify_name(), track.dz.title %>% simplify_name()),
                    artist.dz.quality = stringdist::stringsim(artist.s.name %>% simplify_name(), artist.dz.name %>% simplify_name()),
                    album.dz.quality = stringdist::stringsim(album.s.title %>% simplify_name(), album.dz.title %>% simplify_name())) %>%
      dplyr::arrange(-artist.dz.quality, -track.dz.quality, -album.dz.quality, track.dz.id) %>%
      dplyr::mutate(track.dz.id = track.dz.id %>% as.character()) %>%
      dplyr::first()
  }

  .get_parsed_track_lookup <- function(lookup){
    data.frame(track.dz.id = lookup$id %>% as.character(),
               track.dz.title = lookup$title,
               track.dz.isrc = lookup$isrc,
               track.dz.durationms = lookup$duration * 1000,
               track.dz.rank = lookup$rank,
               track.dz.explicit = lookup$explicit_lyrics,
               track.dz.explicitinfo = lookup$explicit_content_lyrics,
               track.dz.tempo = lookup$bpm,
               track.dz.loudness = lookup$gain) %>%
      dplyr::mutate(track.dz.explicitinfo = purrr::map_chr(.data$track.dz.explicitinfo, decode_explicit_info))
  }

  url <- .create_search_url(track.s.title, artist.s.name, album.s.title)
  result <- .connection_management(url)
  topresult <- .get_parsed_topresult(result)
  if(is.null(topresult)){return(.make_empty_frame())}
  url <- .create_lookup_url(topresult$track.dz.id)
  track_lookup <- .connection_management(url)
  res <- .get_parsed_track_lookup(track_lookup)
  suppressMessages(dplyr::inner_join(res, topresult) %>% dplyr::mutate(track.s.id = track.s.id))
}
