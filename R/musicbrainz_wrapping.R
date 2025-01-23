calculate_and_print_quality <- function(search, found) {
  search <- search %>% simplify_name()
  found <- found %>% simplify_name()
  quality <- stringdist::stringsim(found, search, 'jw')
  cat('search:', search, '\n')
  cat('found:', found, '\n')
  cat('quality:', quality,'\n')
  quality
}

get_top_genres <- function(input, type){
  # input is result frame containing colum type.mb.genres
  # here, all genres must be unlisted and counted, and then joined with the corresponding genre votings
}

get_highest_ranking_genre <- function(tagLookup) {
  highest_tag <- tagLookup$tags %>%
    purrr::lmap(unzip_tags) %>%
    unlist() %>%
    dplyr::tibble(topgenre = .)
  genresMB <- cbind(tagLookup, highest_tag) %>%
    dplyr::select('genres' = 'tags', 'topgenre')
}

unzip_tags <- function(tags) {
  tags <- tags[[1]]
  if (ncol(tags == 2)) {
    tags <- dplyr::filter(tags, .data[['tag_name']] %in% musicbrainzWhitelist$genres)
    tags <- dplyr::arrange(tags, -tag_count)
    return(
      list(tags[['tag_name']][1])
    )
  }
  else {
    return(list(NA))
  }
}

print_linking_success <- function(input, sIDcols) {
  for (sIDcol in sIDcols) {
    mbidCol <- stringr::str_replace(sIDcol, '\\.s\\.', '\\.mb\\.')
    distinctinput <- input %>%
      dplyr::distinct(.data[[sIDcol]], .keep_all = TRUE)
    ratioFound <- nrow(distinctinput %>% tidyr::drop_na(dplyr::all_of(mbidCol))) / nrow(distinctinput)
    cat(paste0('Found ', round(ratioFound,4) * 100, '% of distinct ', sIDcol, ' on Musicbrainz.'), '\n')
  }
}
