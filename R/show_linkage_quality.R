#' Show Linkage Quality
#'
#' Show linkage quality for each database and each quality measure that was calculated available in the input data frame.
#'
#' @param frame Input data frame with linkage results.
#'
#' @return List with plots for each data base for that information is contained in the data frame.
#' @export
#'
#' @examples
#' show_linkage_quality(testresults)
show_linkage_quality <- function(frame){
  .plot_histogram_of_quality <- function(qualityvector){
    frame <- frame %>% dplyr::filter(! is.na(.data[[qualityvector]]))
    plot <- ggplot2::ggplot(frame, ggplot2::aes(x = .data[[qualityvector]]))+
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
    qualityvecs <- colnames(frame) %>%
      stringr::str_subset(paste0(database, '\\..*quality'))
    if(!length(qualityvecs) == 0){
      plot <- .plot_combined_histograms(database)
      plots <- c(plots, list(plot))
    }
  }
  plots
}
