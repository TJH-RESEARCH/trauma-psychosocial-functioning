
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

demographic_tables_2_3 %>% print(n = 300)
