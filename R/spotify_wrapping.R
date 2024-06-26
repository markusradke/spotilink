connect_spotify <- function(pass){
  cat('Connecting with Spotify-API...', '\n')
  Sys.setenv(SPOTIFY_CLIENT_ID = pass[1])
  Sys.setenv(SPOTIFY_CLIENT_SECRET = pass[2])
  cat('Done.\n')
}

get_from_API <- function(input, IDcol, pullFunction, cleanFunction, batchsize) {
  cat('Retrieving data with', substitute(pullFunction) %>% as.character(), 'from identifier', IDcol, '... \n')
  res <- input %>%
    dplyr::distinct(.data[[IDcol]]) %>%
    dplyr::pull(IDcol) %>%
    get_from_IDs(pullFunction, batchsize) %>%
    cleanFunction()
  dplyr::left_join(res, input, ., by = IDcol)
}

get_from_IDs <- function(ids, pullFunction, batchsize) {
  total <- length(ids)
  pulled <- c()
  start <- 1
  stepsize <- batchsize

  repeat{
    stop <- min(c(start + stepsize - 1, total))
    cat("Getting infos for ", start ,"-", stop, " of ", total, "...\n")
    pulled <- pullFunction(ids[start:stop]) %>%
      rbind(pulled, .)
    start <- start + stepsize
    if (start > total) {
      break
    }
  }
  pulled
}
