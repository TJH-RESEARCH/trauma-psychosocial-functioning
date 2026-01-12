# Create a table listing the percentage of bIPF=0 data

table_zeros <-
  bind_rows(
    data_1 %>% 
      mutate(bipf_zero = ifelse(bipf_total == 0, 1, 0)) %>% 
      count(bipf_zero) %>% 
      mutate(perc = n / sum(n), Sample = 1),
    
    data_2 %>% 
      mutate(bipf_zero = ifelse(bipf_total == 0, 1, 0)) %>% 
      count(bipf_zero) %>% 
      mutate(perc = n / sum(n), Sample = 2),
    
    data_3 %>% 
      mutate(bipf_zero = ifelse(bipf_total == 0, 1, 0)) %>% 
      count(bipf_zero) %>% 
      mutate(perc = n / sum(n), Sample = 3)
  ) %>% 
    # Clarify the confusing double negative
    mutate(bIPF = ifelse(bipf_zero == 1, "Zero", "Non-Zero")) %>% 
    select(Sample, bIPF, n, perc)

# Print to console -------------------------------------------------------------
table_zeros %>% print(n = 6)

# Save to file -----------------------------------------------------------------
table_zeros %>% write_csv(here::here("output/table-zeros.csv"))