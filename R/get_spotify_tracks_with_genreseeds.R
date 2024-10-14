#' Get \emph{Spotify}-Recommendations for specific Genres
#'
#'Retrieve a frame with distinct \emph{Spotify} track information, as well for a specific genres through n iterations. Will also return information on the number of \emph{distinct} new tracks added with each recommendation iteration. Each recommendation iteration will retrieve 100 tracks tracks from the API.
#' @param seed \emph{Spotify} genre seed. Run \code{\link{get_available_genre_seeds_spotify}} to get a list of all available genre seeds.
#' @param s_pass Character Vector containing two entries: \emph{Client ID} and \emph{Client secret}. See \url{https://developer.spotify.com/documentation/web-api/concepts/authorization} for details.
#'
#' @param n_iterations Number of recommendation iterations. Defaults to 5.
#'
#' @return List containing the genre seed, a data frame with recommendations, and information (vector and plot) on the number of news tracks added with each recommendation iteration.
#' @export
#'
#' @examples
get_multiple_recommendation_for_genre_seed_spotify <- function(seed, s_pass, n_iterations = 5){
  suppressMessages(connect_spotify(s_pass))
  s_token <- spotifyr::get_spotify_access_token()

  res <- c()
  ndistinct <- c()
  for(i in seq(n_iterations)){
    temp <- get_single_recommendation_for_genre_seed_spotify(100, seed, s_token)
    res <- rbind(res, temp)
    ndistinct <- c(ndistinct, nrow(res %>% dplyr::distinct(track.s.title, track.s.firstartist.name)))
    Sys.sleep(1)
  }
  res <- res %>% dplyr::distinct(track.s.title, track.s.firstartist.name, .keep_all = T)

  news <- c(100, diff(ndistinct))
  ratechanges <- as.data.frame(news %>% as.matrix() %>% t)
  colnames(ratechanges) <- seq(ncol(ratechanges))
  ratechanges$iteration <- seq(nrow(ratechanges))
  ratechanges_long <- tidyr::pivot_longer(ratechanges, cols = !iteration)
  plot <- ggplot2::ggplot(ratechanges_long, ggplot2::aes(x =iteration, y = value, color = name)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = paste0('# distinct new tracks for new recommendations, genre seed: "', seed, '"')) +
    ggplot2::scale_x_continuous(breaks = seq(n_iterations))
  list(seed = seed,
       date_of_retrieval = Sys.Date(),
       recommendations = res,
       n_of_new_recommends_per_iteration = list(new_recommends = news,
                                                plot = plot))
}


get_single_recommendation_for_genre_seed_spotify <- function(n, seed, s_token){
  url <- paste0('https://api.spotify.com/v1/recommendations?limit=', n, '&market=DE&seed_genres=')
  url <- paste0(url, seed)
  url <- paste0(url, '&access_token=', s_token)
  result <- get_api_with_connection_management(url)
  recommendations <- data.frame(track.s.id = sapply(result$tracks, function(hit) hit$id),
                                track.s.title = sapply(result$tracks, function(hit) hit$name),
                                track.s.artists = I(lapply(result$tracks, function(hit) {
                                  artists_df <- data.frame(
                                    external_url = sapply(hit$artists, function(artist) artist$external_urls$spotify),
                                    href = sapply(hit$artists, function(artist) artist$href),
                                    id = sapply(hit$artists, function(artist) artist$id),
                                    name = sapply(hit$artists, function(artist) artist$name),
                                    type = sapply(hit$artists, function(artist) artist$type),
                                    uri = sapply(hit$artists, function(artist) artist$uri),
                                    stringsAsFactors = FALSE
                                  )
                                  return(artists_df)
                                })),
                                track.s.explicit = sapply(result$tracks, function(hit) hit$explicit),
                                track.s.popularity = sapply(result$tracks, function(hit) hit$popularity),
                                track.s.durationms = sapply(result$tracks, function(hit) hit$duration_ms),
                                track.s.albumposition = sapply(result$tracks, function(hit) hit$track_number),
                                album.s.id = sapply(result$tracks, function(hit) hit$album$id),
                                album.s.title = sapply(result$tracks, function(hit) hit$album$name)) %>%
    tidyr::hoist('track.s.artists', track.s.firstartist.id = list('id', 1L), .remove = FALSE) %>%
    tidyr::hoist('track.s.artists', track.s.firstartist.name = list('name', 1L), .remove = FALSE) %>%
    dplyr::mutate(track.s.duration = track.s.durationms * 0.001)

  isrcs <- sapply(result$tracks, function(hit) hit$external_ids$isrc)
  isrcs <- lapply(isrcs, function(x) if (is.null(x)) NA else x)
  recommendations$track.s.isrc <- isrcs

  recommendations <- suppressMessages(get_from_API(recommendations, 'track.s.id',
                                                   spotifyr::get_track_audio_features, clean_features, batchsize = 50))
  recommendations
}


#' Get \emph{Spotify} genre seeds
#'
#'Get a list of all available Spotify genre seeds.
#' @return character vector with all available Spotify genre seeds.
#' @export
#'
#' @examples
#' get_available_genre_seeds_spotify()
get_available_genre_seeds_spotify <- function(){
  connect_spotify(s_pass)
  s_token <- spotifyr::get_spotify_access_token()
  url <- 'https://api.spotify.com/v1/recommendations/available-genre-seeds'
  url <- paste0(url, '?access_token=', s_token)
  genre_seeds <- get_api_with_connection_management(url)
  unname(unlist(genre_seeds))
}
