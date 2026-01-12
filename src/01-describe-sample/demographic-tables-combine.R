
# Combine the demographic tables for Samples 2 and 3
# They can't be combined with Sample 1 because of different varibles/categories

# Gather the tables ------------------------------------------------------------
demographic_tables_2_3 <-
  bind_rows(
    #demographic_table_1 %>% mutate(survey = '1'),
    demographic_table_2 %>% mutate(survey = '2'),
    demographic_table_3 %>% mutate(survey = '3')
    ) %>% 
    pivot_wider(names_from = survey, values_from = c(n, perc)) %>% 
    arrange(variable) %>%
    select(variable, category, contains("1"), contains("2"), contains("3")) 


# Split into General and Military Demographics ---------------------------------
## General Demographics
demographics_general_2_3 <-
  demographic_tables_2_3 %>% 
  filter(variable %in% 
           c("Gender", "Age", 
             "Education Level",
             "Race/Ethnicity",
             "Sexual Orientation"))

## Military Demographics
demographics_military_2_3 <-
  demographic_tables_2_3 %>% 
  filter(!variable %in% 
           c("Gender", "Age", 
             "Education Level",
             "Race/Ethnicity",
             "Sexual Orientation"))


# Print to console -------------------------------------------------------------
demographics_general_2_3 %>% print(n = 100)
demographics_military_2_3 %>% print(n = 300)


# Save to file -----------------------------------------------------------------
demographics_general_2_3 %>% readr::write_csv(here::here('output/demographics-2-3-general.csv'))
demographics_military_2_3 %>% readr::write_csv(here::here('output/demographics-2-3-military.csv'))



