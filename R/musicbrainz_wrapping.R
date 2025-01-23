calculate_and_print_quality <- function(search, found) {
  search <- search %>% simplify_name()
  found <- found %>% simplify_name()
  quality <- stringdist::stringsim(found, search, 'jw')
  cat('search:', search, '\n')
  cat('found:', found, '\n')
  cat('quality:', quality,'\n')
  quality
}

get_top_genres <- function(mbresult, type){
  genres_col <- paste0(type, '.mb.genres')
  topgenre_col <- paste0(type, '.mb.topgenre')
  all_zero_rows <- all(sapply(mbresult[[genres_col]], nrow) == 0)
  if(all_zero_rows){
    return(dplyr::mutate(mbresult, !! topgenre_col := NA))
  }

  overall_genrecounts <- count_genres_in_whole_dataset(mbresult, genres_col)
  mbresult %>%
    dplyr::mutate(!! topgenre_col := purrr::map_chr(mbresult[[genres_col]],
                                                    get_highest_ranking_genre,
                                                    overall_genrecounts))
}

count_genres_in_whole_dataset <- function(mbresult, genres_col){
  pasted_genresframes <- do.call(rbind, mbresult[[genres_col]])
  pasted_genresframes %>%
    dplyr::group_by(tag_name) %>%
    dplyr::summarize(tag_importance = sum(tag_count, na.rm = TRUE)) %>%
    dplyr::filter(tag_name %in% musicbrainzWhitelist$genres)
}

get_highest_ranking_genre <- function(genres, overall_genrecounts) {
  if (ncol(genres == 2)) {
    genres <- suppressMessages(
      dplyr::filter(genres, tag_name %in% musicbrainzWhitelist$genres) %>%
      dplyr::left_join(overall_genrecounts) %>%
      dplyr::arrange(-tag_count, -tag_importance)
      )
    topgenre <- dplyr::first(genres) %>% dplyr::pull(tag_name)
    return(topgenre)
  }
  else {
    return(NA)
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
