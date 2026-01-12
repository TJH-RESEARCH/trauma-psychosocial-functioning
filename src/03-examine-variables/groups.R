# Examine the covariates

## Sample 1: Does impairment level vary by Military Status?
data_1 %>% 
  mutate(military_status = 
           case_when(
             civilian == 1 ~ "Civilian",
             service_member == 0 & civilian == 0 ~ "Veteran",
             service_member == 1 ~ "Service Member",
           )) %>% 
  group_by(military_status) %>% 
  count(bipf_category) %>% 
  mutate(perc = n / sum(n)) %>% 
  pivot_wider(names_from = military_status, values_from = c(n, perc))

