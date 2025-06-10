#' Show Linkage Quality Histograms
#'
#' Show a linkage quality histogram for each database and each quality measure that was calculated available in the input data frame.
#'
#' @param frame Input data frame with linkage results.
#'
#' @return List with plots for each data base for that information is contained in the data frame.
#' @export
#'
#' @examples
#' show_linkage_quality_histograms(testresults)
show_linkage_quality_histograms <- function(input){
  .plot_histogram_of_quality <- function(qualityvector){
    input <- input %>% dplyr::filter(! is.na(.data[[qualityvector]]))
    plot <- ggplot2::ggplot(input, ggplot2::aes(x = .data[[qualityvector]]))+
      ggplot2::geom_histogram(binwidth = 0.025, fill='white', color='black')+
      ggplot2::xlim(c(-0.025,1.025))
    plot
  }

  .plot_combined_histograms <- function(qualityecs){
    database_plots <- list()
    for(vector in qualityvecs){
      temp_plot <- .plot_histogram_of_quality(vector)
      database_plots <- c(database_plots, list(temp_plot))
    }
    plot <- patchwork::wrap_plots(database_plots, nrow = length(database_plots))
    suppressWarnings(print(plot))
    plot
  }
  plots <- list()
  for(database in c('mb', 'dz', 'g', 'dc')){
    qualityvecs <- colnames(input) %>%
      stringr::str_subset(paste0(database, '\\..*quality'))
    if(!length(qualityvecs) == 0){
      plot <- .plot_combined_histograms(database)
      plots <- c(plots, list(plot))
    }
  }
  plots
}


#' Get Critical Linkage Quality Observations to determine a threshold for filtering
#'
#' Get a list of data frames for all platforms and entities (tracks, artists, albums) with
#' respective linking critical observations (quality < 1), arranged in descending quality.
#' With these data frames, thresholds for filtering can be determined manually by eye-balling.
#'
#' @param input
#'
#' @returns List of data frames with critical linkage quality observations for all plattforms and linked entities.
#' @export
#'
#' @examples
#' get_critical_linkage_quality_observations(testresults) #kann man nicht mit testresults testen, neue aus aktuellem projekt nehmen!
get_critical_linkage_quality_observations <- function(input){
  res <- list()
  input_cols <- colnames(input)

  res <- res %>%
    add_platform(input,
                 c('album.dc.quality', 'album.dc.firstartist.quality'),
                 c('album.s.title', 'album.dc.title', 'album.dc.quality',
                   'album.s.firstartist.name', 'album.dc.firstartist.name', 'album.dc.firstartist.quality'),
                 'filter_quality_discogs_albums_dyn') %>%
    add_platform(input,
                 c('track.g.quality', 'track.g.firstartist.quality'),
                 c('track.s.title', 'track.g.title', 'track.g.quality',
                   'track.s.firstartist.name', 'track.g.firstartist.name', 'track.g.firstartist.quality'),
                 'filter_quality_genius_tracks') %>%
    add_platform(input,
                 c('track.dz.quality', 'track.dz.firstartist.quality'),
                 c('track.s.title', 'track.dz.title', 'track.dz.quality',
                   'track.s.firstartist.name', 'track.dz.firstartist.name', 'track.dz.firstartist.quality'),
                 'filter_quality_deezer_tracks') %>%
    add_platform(input,
                 c('album.dz.quality', 'album.dz.firstartist.quality'),
                 c('album.s.title', 'album.dz.title', 'album.dz.quality',
                   'album.s.firstartist.name', 'album.dz.firstartist.name', 'album.dz.firstartist.quality'),
                 'filter_quality_deezer_albums') %>%
    add_platform(input,
                 c('artist.dz.quality'),
                 c('artist.s.name', 'artist.dz.name', 'artist.dz.quality'),
                 'filter_quality_deezer_artists') %>%
    add_platform(input,
                 c('track.dz.quality', 'track.dz.firstartist.quality'),
                 c('track.s.title', 'track.dz.title', 'track.dz.quality',
                   'album.s.title', 'track.dz.album.title', 'track.dz.album.quality',
                   'track.s.firstartist.name', 'track.dz.firstartist.name', 'track.dz.firstartist.quality'),
                 'filter_quality_deezer_all') %>%
    add_platform(input,
                 c('artist.mb.quality'),
                 c('artist.s.name', 'artist.mb.name', 'artist.mb.quality'),
                 'filter_quality_musibrainz_artist-or-all') %>%
    add_platform(input,
                 c('track.mb.quality', 'track.mb.firstartist.quality'),
                 c('track.s.title', 'track.mb.title', 'track.mb.quality',
                   'track.s.firstartist.name', 'track.mb.firstartist.name', 'track.mb.firstartist.quality'),
                 'filter_quality_musicbrainz_acousticbrainzs_tracks-or-all') %>%
    add_platform(input,
                 c('album.mb.quality', 'album.mb.firstartist.quality'),
                 c('album.s.title', 'album.mb.title', 'album.mb.quality',
                   'album.s.firstartist.name', 'album.mb.firstartist.name', 'album.mb.firstartist.quality'),
                 'filter_quality_musicbrainz_albums-or-all')

  if('use_filter_quality_deezer_tracks' %in% names(res) & 'use_filter_quality_deezer_all' %in% names(res)){
    res[['use_filter_quality_deezer_tracks']] <- NULL
  }
  res
}

add_platform <- function(result, input, qualityvecs, selectionvecs, filterfunction){
  if(all(qualityvecs %in% colnames(input))){
    temp <- input %>%
      dplyr::select(dplyr::all_of(selectionvecs)) %>%
      dplyr::filter(! dplyr::if_any(dplyr::all_of(qualityvecs), is.na)) %>%
      dplyr::filter(dplyr::if_any(dplyr::all_of(qualityvecs), function(x) x < 1)) %>%
      dplyr::arrange(dplyr::desc(dplyr::pick(dplyr::all_of(qualityvecs))))
    result[paste0('use_', filterfunction)] <- list(temp)
  }
  result
}
