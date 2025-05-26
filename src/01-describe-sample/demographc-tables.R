
# Demographic Tables

# Education Level ---------------------------------------------------------
data_1 |> 
  count(education_level) |> 
  slice(5,4,1,2,8,6,7,3) |> 
  mutate(`%` = n / sum(n) * 100,
         Variable = 'Education Level') |> 
  rename(Category = 1)

# Employment Status -------------------------------------------------------
data_1 |> 
  count(employment_status) |> 
  slice(1,3,2,4,5) |> 
  mutate(`%` = n / sum(n) * 100,
         Variable = 'Employment Status') |> 
  rename(Category = 1)

# Gender ------------------------------------------------------------------
data_1 |> 
  count(gender_male) |> 
  mutate(gender_male = ifelse(gender_male == 1, 'Male', 'Female')) |> 
  mutate(`%` = n / sum(n) * 100,
         Variable = 'Gender') |> 
  rename(Category = 1)

# Marital Status ----------------------------------------------------------
data_1 |> 
  count(marital_status, sort = TRUE) |> 
  mutate(`%` = n / sum(n) * 100,
         Variable = 'Marital Status') |> 
  slice(1:5,7,6) |> 
  rename(Category = 1)


# Birth_Year ------------------------------------------------------------------
data_1 |> 
  count(Birth_Year) |> 
  mutate(`%` = n / sum(n) * 100,
         Variable = 'Annual Income') |> 
  rename(Category = 1)

# Income ------------------------------------------------------------------
data_1 |> 
  count(income) |> 
  mutate(`%` = n / sum(n) * 100,
         Variable = 'Annual Income') |> 
  rename(Category = 1)

# Military Status ---------------------------------------------------------
bind_rows(
  data_1 |> 
    count(veteran) |> 
    mutate(veteran = ifelse(veteran == 1, 'Veteran', 'Not Veteran')) |> 
    filter(veteran == 'Veteran') |> 
    rename(Category = 1),
  data_1 |> 
    count(service_member) |> 
    mutate(service_member = ifelse(service_member == 1, 'Service Member', 'Not Service Member')) |> 
    filter(service_member == 'Service Member') |> 
    rename(Category = 1),
  data_1 |> 
    count(civilian) |> 
    mutate(civilian = ifelse(civilian == 1, 'Civilian', 'Not Civilian')) |> 
    filter(civilian == 'Civilian') |> 
    rename(Category = 1)
) |> mutate(`%` = n / sum(n) * 100,
            Variable = 'Annual Income') 



# Time in Military ------------------------------------------------------------------
data_1 |> 
  count(time_in_service) |> 
  mutate(`%` = n / sum(n) * 100,
         Variable = 'Time in Service') |> 
  rename(Category = 1)


# Branch ------------------------------------------------------------------
data_1 |> 
  count(service_component) |> 
  mutate(`%` = n / sum(n) * 100,
         Variable = 'service_component') |> 
  rename(Category = 1)



# Pay Grade ---------------------------------------------------------------
data_1 |> 
  count(pay_grade)|> 
  mutate(`%` = n / sum(n) * 100,
         Variable = 'pay_grade') |> 
  rename(Category = 1)



# Times Deployed ---------------------------------------------------------------
data_1 |> 
  count(times_deployed)|> 
  mutate(`%` = n / sum(n) * 100,
         Variable = 'times_deployed') |> 
  rename(Category = 1)

# MOS ---------------------------------------------------------------
data_1 |> 
  count(mos)|> 
  mutate(`%` = n / sum(n) * 100,
         Variable = 'mos') |> 
  rename(Category = 1)

# Head Injury -------------------------------------------------------------
data_1 |> 
  count(hx_head_injury)|> 
  mutate(`%` = n / sum(n) * 100,
         Variable = 'hx_head_injury') |> 
  rename(Category = 1)
    
# Deployed ----------------------------------------------------------------
data_1 |> 
  count(deployed_yorn)|> 
  mutate(`%` = n / sum(n) * 100,
         Variable = 'deployed_yorn') |> 
  rename(Category = 1)


# -------------------------------------------------------------------------
data_1 |> count(trauma_experiences)
data_1 |> count(type_of_trauma)
data_1 |> count(ptsd_tx_received)


# Messed up
data_1 |> count(ptsd_tx_efficacy)
data_1 |> count(date_trauma_exp)
data_1 |> count(ethnicity)


