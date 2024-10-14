connect_spotify <- function(pass){
  message('Connecting with Spotify-API...', '\n')
  Sys.setenv(SPOTIFY_CLIENT_ID = pass[1])
  Sys.setenv(SPOTIFY_CLIENT_SECRET = pass[2])
  message('Done.\n')
}

get_from_API <- function(input, IDcol, pullFunction, cleanFunction, batchsize) {
  fctn_name <- paste0(as.character(substitute(pullFunction))[2],
                                   '_',
                                   as.character((substitute(pullFunction))[3]))
  message(paste0('Retrieving data with ', fctn_name, ' from identifier ', IDcol, '... \n'))
  checkpoint <- read_checkpoint(fctn_name)

  pulled <- checkpoint$saved_data
  start <- checkpoint$last_index
  if(start == 0){start <- 1} else{start <- start + batchsize}
  oldfilename <- checkpoint$last_checkpoint

  res <- input %>%
    dplyr::distinct(.data[[IDcol]]) %>%
    dplyr::pull(IDcol) %>%
    get_from_IDs(pullFunction, batchsize, start, pulled, oldfilename, fctn_name)

  res <- cleanFunction(res)
  res <- dplyr::left_join(res, input, ., by = IDcol)
  message('Done.')
  remove_checkpoints_spotify(fctn_name)
  res
}

get_from_IDs <- function(ids, pullFunction, batchsize, start, pulled, oldfilename, fct_name) {
  total <- length(ids)
  stepsize <- batchsize
  if(start + batchsize > total & start > 1){
    message('Already scraped.')
    return(pulled)
  }
  i <- 1

  repeat{
    stop <- min(c(start + stepsize - 1, total))
    message(paste0("Getting infos for ", start ,"-", stop, " of ", total, "...\n"))
    pulled <- pullFunction(ids[start:stop]) %>%
      rbind(pulled, .)
    filename <- paste0(fct_name, '_', start, '.rds')
    if(i %% 10 == 0){
      saveRDS(pulled, filename)
      if(file.exists(oldfilename)){file.remove(oldfilename)}
      oldfilename <- filename
    }
    start <- start + stepsize
    i <- i + 1
    if (start > total) {
      break
    }
  }
  pulled
}


remove_checkpoints_spotify <- function(fct_name){
  pattern <- paste0('^', fct_name, '_(\\d+)\\.rds$')
  files <- list.files()
  matching_files <- grep(pattern, files, value = TRUE)
  if(length(matching_files) > 0){
    for(file in matching_files){
      file.remove(file)
    }
  }
}
