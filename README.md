# spotilink <img src="man/figures/logo.png" align="right" height="120"/>

<!-- badges: start -->

<!-- badges: end -->

The goal of *spotilink* is to conveniently link information about tracks, albums, and artists from Spotify's Web API to other online music databases through a fuzzy linkage process. Databases currently included in *spotilink* are:

-   Spotify Web API (<https://developer.spotify.com/documentation/web-api/>)

-   Discogs API (<https://www.discogs.com/developers/>)

-   Deezer (<https://developers.deezer.com/login?redirect=/api>)

-   Genius (<https://docs.genius.com/>)

-   MusicBrainz (<https://musicbrainz.org/>) and AcousticBrainz (<https://acousticbrainz.org/>)

spotilink also allows you to retrieve Spotify track IDs from track titles and artist names.

## Installation

You can install the development version of *spotilink* from [GitHub](https://github.com/) with:

``` r
devtools::install_github("markusradke/spotilink")
```

::: {style="color: red"}
**Attention: This package is still under heavy development. Proceed with caution, unfortunately, there still might be plenty of errors.**
:::

## Example

This example shows how to retrieve all the information about Michael Jackson's hit song "Thriller". In order to execute the example, you need to have all the necessary developer tokens.

``` r
library(spotilink)

# get Spotify ID for track
spotify_ids <- get_spotify_ids(artists = 'Michael Jackson', 
                               pass = s_pass, 
                               tracks = 'Thriller', # optional
                               albums = 'Thriller', # optional
                               releaseyear = 1982) # optional 

# get all Spotify information for track
spotify <- get_all_spotify(spotify_ids, s_pass)

# enrich with discogs
discogs <- get_albums_discogs(spotify, dc_pass)

# enrich with deezer
deezer <- get_all_deezer(discogs)

# enrich with genius
genius <- get_tracks_genius(deezer, g_token)

# enrich with musicbrainz
musicbrainz <- get_all_musicbrainz(genius)

# enrich with acousticbrainz
acousticbrainz <- get_tracks_acousticbrainz(musicbrainz)
```
