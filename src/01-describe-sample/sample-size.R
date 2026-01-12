
# Create a table with the sample size for each of the 3 samples
sample_size <-
  tibble(
    Sample = c(1:3),
    N = c(nrow(data_1), nrow(data_2), nrow(data_3))
  )

# Print to console
sample_size %>% print(n = 3)

# Write to file  
sample_size %>% write_csv(here::here("output/sample-size.csv"))
