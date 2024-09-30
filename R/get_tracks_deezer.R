get_tracks_deezer <- function(input, threshold = 0.8){
  are_needed_columns_present(input, c('track.s.id', 'track.s.title', 'track.s.firstartist.name', 'album.s.title'))
  input <- rename_existing_variables(input, geniusLyricsVars)

  input_distinct <- dplyr::distinct(input, track.s.id, track.s.firstartist.name, album.s.title, .keep_all = T)
  if(album.s.title %in% colnames(input)){
    deezer_tracks <- purrr::pmap_df(list(input_distinct$track.s.title,
                                  input_distinct$artist.s.name,
                                  input_distinct$track.s.id,
                                  input_distinct$album.s.title),
                             get_deezer_track, .progress = 'Retrieving tracks from Deezer...')
  }

  message('Done.')
  result <- suppressMessages(dplyr::left_join(input, deezer_tracks))
  print_linkage_for_id(result, 'track.dz.id')
  result
}

get_single_track_deezer <- function(track.s.title, artist.s.name, track.s.id, album.s.title){
  .make_empty_frame <- function(){
    data.frame(track.s.id = track.s.id,
               track.g.id = NA,
               track.g.title = NA,
               track.g.quality = NA,
               track.g.lyrics = NA,
               track.g.lyricsstate = NA,
               artist.g.id = NA,
               artist.g.name = NA,
               artist.g.quality = NA)
  }

  .create_url <- function(track.s.title, artist.s.name, album.s.title = NA){
    paste0('https://api.deezer.com/search?q=artist:"', artist.s.name %>% simplify_name(),
          '" track:"',track.s.title %>% simplify_name(),
          '" album:"', album.s.title %>% simplify_name(), '"') %>% utils::URLencode()
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
    topresults <- data.frame(track.dz.id = sapply(result$data, function(hit) hit$id %>% as.character()),
                             track.dz.title = sapply(result$data, function(hit) hit$title),
                             artist.dz.id = sapply(result$data, function(hit) hit$artist$id %>% as.character()),
                             artist.dz.name = sapply(result$data, function(hit) hit$artist$name),
                             album.dz.id = sapply(result$data, function(hit) hit$album$id %>% as.character()),
                             album.dz.name = sapply(result$data, function(hit) hit$album$name))
    if(nrow(topresults) == 0){return(NULL)}
    topresults %>%
      dplyr::mutate(track.dz.quality = stringdist::stringsim(track.s.title %>% simplify_name(), track.dz.title %>% simplify_name()),
                    artist.dz.quality = stringdist::stringsim(artist.s.name %>% simplify_name(), artist.dz.name %>% simplify_name()),
                    album.dz.quality = stringdist::stringsim(album.s.title %>% simplify_name(), album.dz.title %>% simplify_name())) %>%
      dplyr::arrange(-artist.dz.quality, -track.dz.quality, -album.dz.quality) %>%
      dplyr::first()
  }

  url <- .create_url(track.s.title, artist.s.name, album.s.title)
  result <- .connection_management(url)
  topresult <- .get_parsed_topresult(result)
  # lookup_tracks <- .get_track_lookup(result)
  topresult
}


# erg <- get_single_track_deezer('Like a prayer', 'Madonna', 'testid', 'Like a prayer')
