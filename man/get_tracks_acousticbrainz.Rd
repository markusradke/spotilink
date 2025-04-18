% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_tracks_acousticbrainz.R
\name{get_tracks_acousticbrainz}
\alias{get_tracks_acousticbrainz}
\title{Get \emph{Acousticbrainz} Track Information}
\usage{
get_tracks_acousticbrainz(input)
}
\arguments{
\item{input}{Data Frame containing the following columns:
\itemize{
\item\code{track.mb.id} \cr
with \emph{Musicbrainz} track ids,
\item\code{track.mb.quality} \cr
with \emph{Musicbrainz} track quality, calculated using the Jaro-Winkler distance (ranging from 0 to 1).
}
It is advisable to first run \code{\link{get_tracks_musicbrainz}} before running this command, in order to have all the necessary information.}
}
\value{
Data Frame with added information from the \emph{Acousticbrainz} API using the  \pkg{spotilink} naming convention.
}
\description{
Retrieve a data frame containing information from the \emph{Acousticbrainz} API using the IDs from retrieved from Musicbrainz. Filtering information with \code{\link{filter_quality_musicbrainz_acousticbrainz_tracks}} will be based on the track and firstartist quality of the corresponding Musicbrainz entry.
The result contains information on tracks.
The \pkg{spotilink} naming convention is used.
}
