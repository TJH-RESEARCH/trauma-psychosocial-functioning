
# Prior Predictive Check


# Grid Data ---------------------------------------------------------------
## Create data "grid" similar to the real data, 
## with a sequence of values for the explanatory variable from -1.5 to 4 
## (roughly the min and max of mios_total in data_baked_2 -- in SD not original units)
## and every combination of the dummy-coded covariates.

#min(data_baked_2$mios_total)
#max(data_baked_2$mios_total)

data_sim_2 <-
  expand_grid(
    mios_total = seq(-1.5, 4, by = .25), # Range
    branch = c("Air Force", "Army", "Marines", "Navy", "Public Health Service"),
    sex_female = c(0,1),
    rank = c('E-1 to E-3', 'E-4 to E-6', 'E-7 to E-9', 'W-1 to CW-5', 'O-1 to O-3', 'O-4 to E-6'),
    military_exp_noncombat = c(0,1),
    military_exp_support = c(0,1),
    military_exp_peacekeeping = c(0,1),
    military_exp_combat = c(0,1),
    pc_ptsd_positive_screen = c(0,1),
    race = c("Asian", "Black", "Latino", "Native", "Other", "White"),
    service_era = c("Korea", "Vietnam", "Cold War", "Persian Gulf", "Post 9/11"),
    sexual_orientation = c('Straight', "Bi", "Gay")
  ) %>% 
  
  mutate(
    ## Dummy code categorical variables
    branch_marines = ifelse(branch == 'Marines', 1, 0),
    branch_public_health = ifelse(branch == 'Public Health', 1, 0),
    branch_navy = ifelse(branch == 'Navy', 1, 0),
    branch_air_force = ifelse(branch == 'Air Force', 1, 0),
    
    race_asian = ifelse(race == 'Asian', 1, 0),
    race_black = ifelse(race == 'Black', 1, 0),
    race_latino = ifelse(race == 'Latino', 1, 0),
    race_native = ifelse(race == 'Native', 1, 0),
    race_other = ifelse(race == 'Other', 1, 0),
    
    rank_e1_e3 = ifelse(rank == 'E-1 to E-3', 1, 0),
    rank_e7_e9 = ifelse(rank == 'E-7 to E-9', 1, 0),
    rank_w1_cw5 = ifelse(rank == 'W-1 to CW-5', 1, 0),
    rank_o1_o3 = ifelse(rank == 'O-1 to O-3', 1, 0),
    rank_o4_o6 = ifelse(rank == 'O-4 to O-6', 1, 0),
    
    service_era_korea = ifelse(service_era == 'Korea', 1, 0),
    service_era_vietnam = ifelse(service_era == 'Vietnam', 1, 0),
    service_era_cold_war = ifelse(service_era == 'Cold War', 1, 0),
    service_era_persian_gulf = ifelse(service_era == 'Persian Gulf', 1, 0),
    
    sexual_orientation_bi = ifelse(sexual_orientation == 'Bi', 1, 0),
    sexual_orientation_gay = ifelse(sexual_orientation == 'Gay', 1, 0),
    
    ## and transform the mios_total SD to original units, saved in a separate variable
    mios_total_og = mios_total * sd(data_2$mios_total) + mean(data_2$mios_total)
  ) %>% 
  
  # Randomly sample from the grid....because otherwise the table is too large
  slice_sample(n = 2e4)
  


# Linear Predictor --------------------------------------------------------
## Calculate the "linear predictor" i.e.,  μ = βX + α  , on log scale
## using the simulated grid data above and predictions from
## the model fit to the priors only. In other words, predict this model μ given 
## the prior specifications of β and α
## and with X = the range of possible observable values.

linpred_prior_2 <- 
  posterior_linpred(model_2_hurdle_prior, 
                    newdata = data_sim_2, 
                    dpar = "mu", 
                    transform = FALSE)

# Summarize across draws to get a single estimate per row (mean of the posterior)
data_sim_2$log_mu <- colMeans(linpred_prior_2)

# compute posterior mean on outcome scale (not the log scale)
data_sim_2$mu <- exp(data_sim_2$log_mu)


# Plot linear predictor (Response Scale) ---------------------------------------
plot_prior_predictive_2 <-
ggplot(data_sim_2, aes(x = mios_total_og, y = mu)) +
  geom_smooth(method = 'loess', color = colors_tam[4]) +
  labs(
    title = "Sample from weakly informative prior",
    subtitle = "With beta around 0, the effect should be flat.",
    x = "MIOS Total", 
    y = "Linear Predictor (μ)",
    caption = "This basically assumes most people would have a dysfunction around 10<br>if none of the variables (explanatory or covariates) had a consistent effect."
    ) +
  lims(y = c(0,100), x = c(0,50)) +  # to put on the scale of the outcome variable 0-100
  theme_scatter


## Print to console ------------------------------------------------------------
plot_prior_predictive_2 %>% print()


## Save to file ----------------------------------------------------------------
ggsave(plot = plot_prior_predictive_2, file = here::here("output/plot-prior-predictive-2.jpg"), width = 6, height = 4)


# Plot prior draws for coefficients  -------------------------------------------
plot_prior_draws_2 <-
  as_draws_df(model_2_hurdle_prior) %>%
  select(starts_with("b_")) %>%
  pivot_longer(cols = everything(), names_to = "term", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 50) +
  facet_wrap(~term, scales = "free") +
  labs(title = "Prior Draws for Coefficients") +
  lims(x = c(-12, 12), y = c(0, 3500)) + 
  scale_x_continuous(limits =c(-12,12),breaks = c(-10, 0, 10)) +
  theme_scatter +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_blank())


# Print to console --------------------------------------------------------
plot_prior_draws_2 %>% print()

# Save to file ------------------------------------------------------------
ggsave(plot = plot_prior_draws_2, file = here::here("output/priors/plot-prior-draws-2.jpg"), width = 8, height = 8)

# Remove objects from environment -----------------------------------------
rm(plot_prior_draws_2, data_sim_2, plot_prior_predictive_2, linpred_prior_2)
