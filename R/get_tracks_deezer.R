# get_tracks_deezer <- function(input, de_pass, threshold = 0.8){
#
# }
#
# Tests Deezer

#
# library(devtools)
# library(jsonlite)
#
# url <- urltools::url_encode(url)
# res <- jsonlite::fromJSON(url)
#
#
# library(httr)
#
# # Define the URL
#
# # Make the GET request
# response <- GET(url)
# url <- 'https://api.deezer.com/search?q=artist:%22aloe%20blacc%22%20track:%22i%20need%20a%20dollar%22'
# url <- 'https://api.deezer.com/search?q=artist:%22aloe%20blacc'
# url <- 'https://api.deezer.com/search?q=artist:%22aloe%20blacc%22%20track:%22i%20need%20a%20dollar%22'
#
# # Check the status code
# if (status_code(response) == 200) {
#   content <- content(response, "parsed")
#   print(content)
# } else {
#   print(paste("Error:", status_code(response), content(response, "text")))
# }
# erg <- content$data[[1]] %>% as.data.frame()
# dplyr::glimpse(erg)
