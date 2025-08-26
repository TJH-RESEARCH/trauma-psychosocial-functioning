
sample_size <-
  tibble(
    Survey = c(1:3),
    N = c(nrow(data_1), nrow(data_2), nrow(data_3))
  )

sample_size %>% write_csv(here::here("output/sample-size.csv"))
