
# Combining the 2nd and 3rd surveys is possible, 
# however the 1st survey is quite different in its demographic varibales/categories

demographic_tables_2_3 <-
  bind_rows(
    #demographic_table_1 %>% mutate(survey = '1'),
    demographic_table_2 %>% mutate(survey = '2'),
    demographic_table_3 %>% mutate(survey = '3')
    ) %>% 
    pivot_wider(names_from = survey, values_from = c(n, perc)) %>% 
    arrange(variable) %>%
    select(variable, category, contains("1"), contains("2"), contains("3")) 


# Split into General and Military Demographics ----------------------------
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



# Print -------------------------------------------------------------------
demographics_general_2_3 %>% print(n = 100)
demographics_military_2_3 %>% print(n = 300)


# Save --------------------------------------------------------------------
demographics_general_2_3 %>% readr::write_csv(here::here('output/demographics-2-3-general.csv'))
demographics_military_2_3 %>% readr::write_csv(here::here('output/demographics-2-3-military.csv'))

# Message -----------------------------------------------------------------
message('Demographic tables saved to `output/`')





