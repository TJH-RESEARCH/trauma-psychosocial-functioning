library(modelr)
library(brms)

# Prior Predictive -------------------------------------------------------------

## Simulate some data
data_sim_2 <-
  expand_grid(
    mios_total = seq(-1.5, 3.5, by = .2), 
    pc_ptsd_positive_screen = c(0,1),
    military_exp_combat = c(0,1),
    military_exp_noncombat = c(0,1),
    military_exp_support = c(0,1),
    military_exp_peacekeeping = c(0,1),
    sex_female = c(0,1),
    sexual_orientation = c("straight", "gay", "bi"),
    race_asian = c(0,1),
    race_black = c(0,1),
    race_latino = c(0,1),
    race_native = c(0,1),
    race_other = c(0,1),
    rank = c('E-1 to E-3', 'E-4 to E-6', 'E-7 to E-9', 'W-1 to CW-5', 'O-1 to O-3', 'O-4 to O-6'),
    service_era = c('Korea', 'Cold War', 'Vietnam', 'Persian Gulf', 'Post 911'),
    branch = c('Air Force', 'Army', 'Marines', 'Navy', 'Public Health')
  ) %>% 
  
  # Dummy Variables for Categorical
  mutate(
    sexual_orientation_gay = ifelse(sexual_orientation == 'gay', 1, 0),
    sexual_orientation_bi  = ifelse(sexual_orientation == 'bi', 1, 0),
    rank_e1_e3 = ifelse(rank == 'E-1 to E-3', 1, 0),
    rank_e7_e9  = ifelse(rank == 'E-7 to E-9', 1, 0),
    rank_w1_cw5 = ifelse(rank == 'W-1 to CW-5', 1, 0),
    rank_o1_o3 = ifelse(rank == 'O-1 to O-3', 1, 0),
    rank_o4_o6 = ifelse(rank == 'O-4 to O-6', 1, 0),
    service_era_korea = ifelse(service_era == "Korea", 1, 0),
    service_era_cold_war = ifelse(service_era == "Cold War", 1, 0),
    service_era_vietnam = ifelse(service_era == "Vietnam", 1, 0),
    service_era_persian_gulf = ifelse(service_era == "Persian Gulf", 1, 0),
    branch_air_force = ifelse(branch == 'Air Force', 1, 0),
    branch_marines = ifelse(branch == 'Marines', 1, 0),
    branch_navy = ifelse(branch == 'Navy', 1, 0),
    branch_public_health = ifelse(branch == 'Public Health', 1, 0)
  ) %>% 
  slice_sample(n = 10000)


# Get linear predictor on log scale
linpred_mat <- posterior_linpred(model_2_prior, 
                                 newdata = data_sim_2, 
                                 dpar = "mu", 
                                 transform = FALSE)

# Summarize across draws to get a single estimate per row (mean of the posterior)
data_sim_2$log_mu <- colMeans(linpred_mat)

# compute posterior mean on outcome scale
data_sim_2$mu <- exp(data_sim_2$log_mu)

# Plot linear predictor (log-mu)
ggplot(data_sim_2, aes(x = mios_total, y = log_mu)) +
  geom_point(alpha = 0.75) +
  labs(y = "Linear Predictor (log-mu)")

# Plot linear predictor (log-mu)
ggplot(data_sim_2, aes(x = mios_total, y = mu)) +
  geom_point(alpha = 0.75) +
  labs(
    title = "Sample from weakly informative prior",
    subtitle = "With beta around 0, the effect should be flat. Assume most people have a baseline functioning around 10 without any PTSD",
    y = "Linear Predictor (outcome scale)") +
  lims(y = c(0,100)) # to put on the scale of the outcome variable 0-100

  
## Prior draws for coefficients. 
as_draws_df(model_2_prior) %>%
  select(starts_with("b_")) %>%
  pivot_longer(cols = everything(), names_to = "term", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 50) +
  facet_wrap(~term, scales = "free") +
  labs(title = "Prior Draws for Coefficients")



