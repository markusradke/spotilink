% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_artists_spotify.R
\name{get_artists_spotify}
\alias{get_artists_spotify}
\title{Get \emph{Spotify} API Information on Artists}
\usage{
get_artists_spotify(input, pass)
}
\arguments{
\item{input}{Data Frame containing a column \code{artist.s.id} with \emph{Spotify} artist ids.}

\item{pass}{Character Vector containing two entries: \emph{Client ID} and \emph{Client secret}. See \url{https://developer.spotify.com/documentation/web-api/concepts/authorization} for details.}
}
\value{
Data Frame with added information from the \emph{Spotify} Web API using the \pkg{spotilink} naming convention.
}
\description{
Retrieve a data frame containing information from the \emph{Spotify} API.
The result contains information on the artists using the  \pkg{spotilink} naming convention.
}
