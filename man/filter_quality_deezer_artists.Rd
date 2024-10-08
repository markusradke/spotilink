% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_low_quality.R
\name{filter_quality_deezer_artists}
\alias{filter_quality_deezer_artists}
\title{Filter low quality results for \emph{Deezer} Artists}
\usage{
filter_quality_deezer_artists(frame, artist_threshold)
}
\arguments{
\item{frame}{Data frame containing Deezer artist variables.}

\item{artist_threshold}{Threshold for Deezer artist quality. Ranging between 0 and 1.}
}
\value{
Updated input frame with all Deezer artist information set to NA where at least one threshold was not met.
}
\description{
Filter low quality results for \emph{Deezer} Artists
}
