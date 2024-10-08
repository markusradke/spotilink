% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_artists_musicbrainz.R
\name{get_artists_musicbrainz}
\alias{get_artists_musicbrainz}
\title{Get \emph{MusicBrainz} Artist Information}
\usage{
get_artists_musicbrainz(input, artist_threshold = 0.8)
}
\arguments{
\item{input}{Data Frame containing the following columns:
\itemize{
\item \code{artist.s.id} \cr
with \emph{Spotify} artist id,
\item \code{artist.s.name} \cr
with \emph{Spotify} artist name.
}
It is advisable to first run \code{\link{get_artists_spotify}} before running this command,
in order to have all the necessary information.}

\item{artist_threshold}{Floating point number between 0 and 1 indicating which elements to keep that were found using a fuzzy search.
The values correspond to the string similarity (1 - Jaro-Winkler distance) between the artist name on \emph{Spotify} and the found artist name on \emph{Musicbrainz}.}
}
\value{
Data Frame with added information from the \emph{MusicBrainz} API using the  \pkg{spotilink} naming convention.
}
\description{
Retrieve a data frame containing information from the \emph{MusicBrainz} API.
The result contains information on artists.
The \pkg{spotilink} naming convention is used.
}
\examples{
data <- data.frame(artist.s.id = c('5aIqB5nVVvmFsvSdExz408'),
                  artist.s.name = c('Johann Sebastian Bach'))

get_artists_musicbrainz(data)
}
