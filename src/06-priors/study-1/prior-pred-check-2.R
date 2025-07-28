
data_sim <-
  expand_grid(
    pcl_total = seq(0, 3.75, by = .05), 
    deployed = c(0,1),
    gender_female = c(0,1),
    race = c('asian', 'black', 'native', 'other', 'white'),
    rank = c('e1_e4', 'e5_e6', 'e7_e9', 'o1_o3_w1_cw2', 'o4_o6_cw3_cw5'),
    mos = c('combat', 'combat_support', 'combat_service_support', 'operations', 'logistics', 'support', 'medical'),
    branch = c('airforce', 'army', 'marines')
  ) %>% 
  
  # Dummary Dummy Variables for Categorical
  mutate(
    race_asian = ifelse(race == 'asian', 1, 0),
    race_black = ifelse(race == 'black', 1, 0),
    race_native = ifelse(race == 'native', 1, 0),
    race_other = ifelse(race == 'other', 1, 0),
    rank_e1_e4 = ifelse(rank == 'e1_e4', 1, 0),
    rank_e7_e9 = ifelse(rank == 'e7_e9', 1, 0),
    rank_o1_o3_w1_cw2 = ifelse(rank == 'o1_o3_w1_cw2', 1, 0),
    rank_o4_o6_cw3_cw5 = ifelse(rank == 'o4_o6_cw3_cw5', 1, 0),
    mos_combat = ifelse(mos == 'combat', 1, 0),
    mos_combat_support = ifelse(mos == 'combat_support', 1, 0),
    mos_combat_service_support = ifelse(mos == 'combat_service_support', 1, 0),
    mos_operations = ifelse(mos == 'operations', 1, 0),
    mos_logistics = ifelse(mos == 'logistics', 1, 0),
    mos_support = ifelse(mos == 'support', 1, 0),
    mos_medical = ifelse(mos == 'medical', 1, 0),
    branch_airforce = ifelse(branch == 'airforce', 1, 0),
    branch_marines = ifelse(branch == 'marines', 1, 0)
  ) %>% 
  sample_n(size = 3000, replace = FALSE)
