get_audioanalysis_spotify <- function(input, pass) {
  connect_spotify(pass)
  # res <- rename_existing_variables(input, 'spotify_audioanalysis') #TODO implement single renaming requests
  # get_from_API(res, 'track.s.id', get_track_audio_analysis, clean_analysis, batchsize = 1) # TODO later
}


clean_analysis <- function(analysisRaw) {
  dplyr::tibble(meta = list(analysisRaw$meta),
                track = list(analysisRaw$track),
                bars = list(analysisRaw$bars),
                beats = list(analysisRaw$beats),
                sections = list(analysisRaw$sections),
                segments = list(analysisRaw$segments),
                tatums = list(analysisRaw$tatums)) %>%
    dplyr::mutate(track.s.id = 'x') %>% # todo incorporate id and make data frame
    dplyr::select('track.s.id', dplyr::everything())
}
