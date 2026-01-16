# Create demographic tables for Sample 1

demographic_table_1 <-
  bind_rows(
    
    # Military Status ----------------------------------------------------------
    bind_rows(
      data_1 %>% 
        count(veteran) %>% 
        mutate(veteran = ifelse(veteran == 1, 'Veteran', 'Not Veteran')) %>% 
        filter(veteran == 'Veteran') %>% 
        rename(category = 1),
      data_1 %>% 
        count(service_member) %>% 
        mutate(service_member = ifelse(service_member == 1, 'Service Member', 'Not Service Member')) %>% 
        filter(service_member == 'Service Member') %>% 
        rename(category = 1),
      data_1 %>% 
        count(civilian) %>% 
        mutate(civilian = ifelse(civilian == 1, 'Civilian', 'Not Civilian')) %>% 
        filter(civilian == 'Civilian') %>% 
        rename(category = 1)
    ) %>% mutate(perc = n / sum(n),
                variable = 'Military Status'),
  

  # Gender ---------------------------------------------------------------------
  data_1 %>% 
    count(gender_male) %>% 
    mutate(gender_male = ifelse(gender_male == 1, 'Male', 'Female')) %>% 
    mutate(perc = n / sum(n),
           variable = 'Gender') %>% 
    rename(category = 1),
  
  # Birth_Year -----------------------------------------------------------------
  data_1 %>% 
    count(birth_year) %>% 
    mutate(perc = n / sum(n),
           variable = 'Birth Year') %>% 
    rename(category = 1),
  
  # Education Level ------------------------------------------------------------
  data_1 %>% 
    count(education_level) %>% 
    slice(5,4,1,2,8,6,7,3) %>% 
    mutate(perc = n / sum(n),
           variable = 'Education Level') %>% 
    rename(category = 1),
  
  # Income ---------------------------------------------------------------------
  data_1 %>% 
    count(income) %>% 
    mutate(perc = n / sum(n),
           variable = 'Annual Income') %>% 
    rename(category = 1),
  
  # Employment Status ----------------------------------------------------------
  data_1 %>% 
    count(employment_status) %>% 
    slice(1,3,2,4,5) %>% 
    mutate(perc = n / sum(n),
           variable = 'Employment Status') %>% 
    rename(category = 1),
  
  # Marital Status -------------------------------------------------------------
  data_1 %>% 
    count(marital_status, sort = TRUE) %>% 
    mutate(perc = n / sum(n),
           variable = 'Marital Status') %>% 
    slice(1:5,7,6) %>% 
    rename(category = 1),
  
  # Branch ---------------------------------------------------------------------
  data_1 %>% 
    count(service_component) %>% 
    mutate(perc = n / sum(n),
           variable = 'Branch') %>% 
    rename(category = 1),
  
  # Pay Grade ------------------------------------------------------------------
  data_1 %>% 
    count(pay_grade)%>% 
    mutate(perc = n / sum(n),
           variable = 'Highest Paygrade Achieved') %>% 
    rename(category = 1),
  
  # Time in Military -----------------------------------------------------------
  data_1 %>% 
    count(time_in_service) %>% 
    mutate(perc = n / sum(n),
           variable = 'Years of Service') %>% 
    rename(category = 1),
  
  # Times Deployed -------------------------------------------------------------
  data_1 %>% 
    count(times_deployed)%>% 
    mutate(perc = n / sum(n),
           variable = 'Times Deployed') %>% 
    rename(category = 1),
  
  # MOS ---------------------------------------------------------------
  data_1 %>% 
    count(mos)%>% 
    mutate(perc = n / sum(n),
           variable = 'MOS') %>% 
    rename(category = 1),
  
  # Head Injury -------------------------------------------------------------
  data_1 %>% 
    count(hx_head_injury)%>% 
    mutate(perc = n / sum(n),
           variable = 'History of Head Injury',
           hx_head_injury = ifelse(hx_head_injury == 0, "No", "Yes")
           ) %>% 
    rename(category = 1),
      
  # Deployed ----------------------------------------------------------------
  data_1 %>% 
    mutate(deployed_yorn = ifelse(civilian == 1, NA, deployed_yorn)) %>% 
    count(deployed_yorn)%>% 
    mutate(perc = n / sum(n),
           variable = 'Previous Deployment') %>% 
    rename(category = 1)
  
  ) %>% 
  select(variable, everything())


# Split into General and Military Demographics ---------------------------------
general_demos <- c(
  "Education Level", "Employment Status", "Gender", 
  "Marital Status", "Birth Year", "Annual Income", 
  "Military Status", "History of Head Injury"
)

## General Demographics
demographics_general_1 <-
  demographic_table_1 %>% 
  filter(variable %in% general_demos)

## Military Demographics
demographics_military_1 <-
  demographic_table_1 %>% 
  filter(!variable %in% general_demos)


# Print to console -------------------------------------------------------------
demographics_general_1 %>% print(n = 100)
demographics_military_1 %>% print(n = 100)

# Save to file -----------------------------------------------------------------
demographics_general_1 %>% write_csv(here::here('output/demographics/demographics-1-general.csv'))
demographics_military_1 %>% write_csv(here::here('output/demographics/demographics-1-military.csv'))

# Remove objects from environment
rm(demographics_general_1)
rm(demographics_military_1)

