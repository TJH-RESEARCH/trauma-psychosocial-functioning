


# Creates a helper function to count the percentage of demographics

count_perc <-
  function(x, sort){
    x = 
      x %>% 
      count(sort = sort) %>% 
      ungroup() %>% 
      mutate(perc = n / sum(n) * 100) %>% 
      rename(category = 1)
    
    if(!is.character(x$category)){
      x = x %>% mutate(category = as.character(category))
    }
    
    return(x)
    
  }

create_percentage_table <- function(data){
  
  data %>% 
    tidyr::pivot_longer(everything(), names_to = 'category', values_to = 'response') %>% 
    group_by(category) %>% 
    summarize(total = n(), n = sum(response, na.rm = T), perc = n/total * 100) %>% 
    select(!total)
}



demographic_table_3 <-
  bind_rows( 
    
    # Age ---------------------------------------------------------------------
    data_3 %>% 
      select(age_screener) %>% 
      mutate(category = 
               cut(data_3$age, 
                   breaks = c(18, 34.5, 54.5, 64.5, 74.5, 100))) %>% 
      count(category) %>% 
      mutate(perc = n / sum(n) * 100, 
             category = c("18 to 34",
                          "35 to 54",
                          "55 to 64",
                          "65 to 74",
                          "75 and older"),
             variable = "Age"),
    
    
    # Branch ------------------------------------------------------------------
    data_3 %>% 
      group_by(branch) %>% 
      count_perc(TRUE) %>% 
      mutate(variable = "Branch"),
    

    # Discharge Reason --------------------------------------------------------
    data_3 %>% 
      group_by(discharge_reason) %>% 
      count_perc(TRUE) %>% 
      mutate(variable = "Discharge Reason"),
    
    
    # Education ----------------------------------------------------------------
    data_3 %>% 
      group_by(education) %>% 
      count(sort = F) %>% 
      ungroup() %>% 
      mutate(perc = n / sum(n) * 100) %>% 
      rename(category = 1) %>% 
      mutate(variable = "Education Level"),
    
    
    # Military Experiences ----------------------------------------------------
    data_3 %>% 
      select(starts_with('military_exp') & !ends_with('total')) %>% 
      create_percentage_table() %>% 
      slice(c(2,3,5,6,1,4)) %>% 
      mutate(variable = "Military Experiences",
             category = c("Combat Depoloyment",
                          "Non-combat Deployment",
                          "Peacekeeping or Humanitarian Deployment",
                          "Combat Support",
                          "Any of the above",
                          "None of the above")),
    
    # Military Family ---------------------------------------------------------
    data_3 %>% 
      select(starts_with('military_family') & !ends_with('total')) %>% 
      create_percentage_table() %>% 
      slice(c(5,6,7,2,4,1,3)) %>% 
      mutate(variable = "Military Family",
             category = c("Parent", 
                          "Sibling", 
                          "Spouse", 
                          "Child", 
                          "Other family member", 
                          "Any of the above", 
                          "None of the above")),
    
    
    # Race ----------------------------------------------------------------------
    data_3 %>% 
      group_by(race) %>% 
      count_perc(TRUE) %>% 
      mutate(variable = "Race/Ethnicity"),
    
    
    # Highest Paygrade Achieved ------------------------------------------------
    data_3 %>% 
      select(starts_with('rank')) %>% 
      create_percentage_table() %>% 
      slice(c(1,2,3,6,4,5)) %>%
      mutate(variable = "Highest Paygrade Achieved",
             category = c("E-1 to E-3",
                          "E-4 to E-6",
                          "E-7 to E-9",
                          "W-1 to CW-5",
                          "O-1 to O-3",
                          "O-4 to O-6")),
    
    
    # Service Era -------------------------------------------------------------
    data_3 %>% 
      select(service_era_init) %>% 
      tidyr::pivot_longer(everything(), names_to = 'category', values_to = 'response') %>% 
      count(response) %>% 
      mutate(perc = n/sum(n) * 100) %>% 
      rename(category = 1) %>% 
      mutate(variable = "Initial Service Era"),
    
    # Gender ----------------------------------------------------------------------
      data_3 %>% 
      group_by(gender) %>% 
      count_perc(sort = TRUE) %>% 
      mutate(variable = "Gender", 
             category = case_when(category == "male" ~ "Male",
                                  category == "female" ~ "Female",
                                  .default = category
             )),
    

    # Years of Service --------------------------------------------------------
      data_3 %>% 
      select(years_served) %>% 
      mutate(group = 
               cut(data_3$years_served, 
                   breaks = c(0, 4.9, 9.9, 14.9, 19.9, 40))) %>% 
      count(group) %>% 
      ungroup() %>% 
      mutate(perc = n / sum(n) * 100,
             category = c("0 to 4",
                          "5 to 9",
                          "10 to 14",
                          "15 to 20",
                          "20 or more"),
             variable = "Years of Service") %>% 
      select(category, !group),
    

    # Years since separation --------------------------------------------------
      data_3 %>% 
      select(years_discharged) %>% 
      mutate(group = 
               cut(data_3$years_discharged, 
                   breaks = c(-1, 9.9, 19.9, 29.9, 39.9, 49.9, 100))) %>% 
      count(group) %>% 
      ungroup() %>% 
      mutate(perc = n / sum(n) * 100,
             category = c("0 to 9",
                          "10 to 19",
                          "20 to 29",
                          "30 to 39",
                          "40 to 50",
                          "50 or more"),
             variable = "Years since Separation") %>% 
      select(category, !group)
    
    
  ) %>% 
  #mutate(perc = paste0(round(perc, 1), "%")) %>% 
  select(variable, everything())



# Print -------------------------------------------------------------------
demographic_table_3 %>% print(n = 100)

# Save --------------------------------------------------------------------
demographic_table_3 %>% readr::write_csv(here::here('output/demographic-table-3.csv'))

# Message -----------------------------------------------------------------
message('Demographic table saved to `output/demographic-table-3.csv`')

