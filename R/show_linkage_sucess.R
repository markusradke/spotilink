#' Show Linkage Success
#'
#' Show linkage success for each database that was linked to \emph{Spotify} with \code{spotilink}. The success calculated for distinct entities within the data is based on the entity that was used searched for on the data base (e.g., for \emph{Discogs} the success is calculated for distinct albums, while it is calculated for distinct tracks for \emph{Genius}). Also returns the relative frequency of complete linkage for tracks.
#'
#' @param frame Input data frame with linkage results.
#'
#' @return Data frame with the relative frequencies of linked entities within the data for all databases that were linked to the original \emph{Spotify} data.
#' @export
#'
#' @examples
#' show_linkage_success(testresults)
show_linkage_success <- function(frame){
  idvecs <- colnames(frame)[colnames(frame) %in% c('track.s.id',
                                                   'album.s.id',
                                                   'artist.s.id',
                                                   'track.mb.id',
                                                   'album.mb.id',
                                                   'artist.mb.id',
                                                   'track.dz.id',
                                                   'album.dc.id',
                                                   'track.g.id',
                                                   'album.dz.id',
                                                   'artist.dz.id',
                                                   'track.ab.id')]
  res <- suppressMessages(purrr::map_df(idvecs, print_linkage_for_id, frame))
  complete <- get_complete_linkage(frame, idvecs)
  res <- rbind(res, complete)
  create_linkage_table(res)
}


print_linkage_for_id <- function(idcol, frame){
  message(paste0('Showing linkage for ', idcol), ':')
  entity <- stringr::str_extract(idcol, '(album|track|artist)')
  frame_distinct <- dplyr::distinct(frame, .data[[paste0(entity, '.s.id')]], .keep_all = T)

  freq_na <- nrow(dplyr::filter(frame, ! is.na(.data[[idcol]])))
  relfreq_na <- freq_na / nrow(frame)
  relfreq_na_percent <- 100 * round(relfreq_na, 4)
  freq_na_distinct <- nrow(dplyr::filter(frame_distinct, ! is.na(.data[[idcol]])))
  relfreq_na_distinct <- freq_na_distinct / nrow(frame_distinct)
  relfreq_na_percent_distinct <- 100 * round(relfreq_na_distinct, 4)


  message(paste0('Found ', relfreq_na_percent_distinct, '% of distinct ', entity, 's in the data set.\n',
                 'This equals to ', relfreq_na_percent, '% of all ', entity, 's in the data set.'))
  data.frame(database = idcol,
             'freq' = freq_na,
             'distfreq' = freq_na_distinct,
             'relfreq' = relfreq_na_percent,
             'distrelfreq' = relfreq_na_percent_distinct)
}


get_complete_linkage <- function(frame, idvecs){
  complete <- frame
  for(id in idvecs){
    complete <- complete %>% dplyr::filter(!is.na(.data[[id]]))
  }
  n_complete <- nrow(complete)
  n_complete_distinct <- nrow(complete %>% dplyr::distinct(track.s.id))
  perc_complete <- round(n_complete / nrow(frame), 4) * 100
  perc_complete_distinct <- round(n_complete_distinct / nrow(frame %>% dplyr::distinct(track.s.id)), 4) * 100
  data.frame(database = 'complete track linkage',
             'freq' = n_complete,
             'distfreq' = n_complete_distinct,
             'relfreq' = perc_complete,
             'distrelfreq' = perc_complete_distinct)
}



create_linkage_table <- function(linkage){
  prepared_data <- linkage %>%
    dplyr::mutate(Layer = stringr::str_extract(database, '(track|album|artist)') %>% stringr::str_to_title() %>% paste0('s'),
                  Source = stringr::str_extract(database, '(?<=\\.).*(?=\\.)'),#
                  Source = ifelse(Source == 's', 'Spotify', Source),
                  Source = ifelse(Source == 'mb', 'Musicbrainz', Source),
                  Source = ifelse(Source == 'ab', 'Acousticbrainz', Source),
                  Source = ifelse(Source == 'dz', 'Deezer', Source),
                  Source = ifelse(Source == 'dc', 'Discogs', Source),
                  Source = ifelse(Source == 'g', 'Genius', Source),
                  Source = ifelse(database == 'complete track linkage', 'Complete track linkage', Source),
                  relfreq = ifelse(Source == 'Spotify', ' ', relfreq),
                  distrelfreq = ifelse(Source == 'Spotify', ' ', distrelfreq)) %>%
    dplyr::select(Source,
                  freq,
                  relfreq,
                  distfreq,
                  distrelfreq,
                  Layer)

  gt::gt(prepared_data, groupname_col = 'Layer') %>%
    gt::cols_label(
      freq = gt::html('Frequency<br> n'),
      relfreq = gt::html('Rel. Frequency<br>[%]'),
      distfreq = gt::html('Frequency<br>n'),
      distrelfreq = gt::html('Rel. Frequency<br>[%]'),
      Source = ''
    ) %>%
    gt::tab_spanner('Whole Dataframe', c(freq, relfreq)) %>%
    gt::tab_spanner('Distinct', c(distfreq, distrelfreq)) %>%
    gt::cols_align(align = 'center', columns = -Source) %>%
    gt::row_group_order(c('Albums', 'Artists', 'Tracks')) %>%
    gt::tab_style(style = list(gt::cell_text(weight = 'bold')),
                  locations = list(gt::cells_row_groups(),
                                   gt::cells_column_spanners())) %>%
    gt::tab_style_body(style = gt::cell_text(weight='bold'),
                       columns = Source,
                       value = 'Complete track linkage')
}


