test_that('can handle NA ids', {
  res <- suppressWarnings(get_all_spotify(data.frame(track.s.id = NA, artist.search = 'Peter Fox'), s_pass))
  res <<- res
  suppressWarnings(expect_setequal(res, make_na_frame_spotify_all() %>% dplyr::mutate(artist.search = 'Peter Fox')))

  res <- suppressWarnings(get_all_spotify(data.frame(track.s.id = c(NA, NA), artist.search = c('Peter Fox', 'Das Vokalprojekt')), s_pass))
  suppressWarnings(expect_setequal(res, make_na_frame_spotify_all(NA) %>%
                 rbind(make_na_frame_spotify_all(NA)) %>%
                 dplyr::mutate(artist.search =  c('Peter Fox', 'Das Vokalprojekt'))))

})

# get_spotify_ids('Enzo Sifredi', tracks = 'sometimes', s_pass) %>% get_all_spotify()
