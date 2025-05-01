
# Compare Estimates -------------------------------------------------------
results <-
  bind_rows(

    
  ## Hurdle Gamma Models  
  hurdle_default_ptsd_bivariate %>% tidy_draws() %>% 
      mutate(
        model = 'hurdle', 
        prior = 'default',
        explanatory_var = 'ptsd', 
        variables = 'bivariate'),
    hurdle_default_ptsd_adjustment %>% tidy_draws() %>% 
      mutate(
        model = 'hurdle', 
        prior = 'default',
        explanatory_var = 'ptsd', 
        variables = 'adjustment_set'),
    hurdle_default_ptsd_controls %>% tidy_draws() %>% 
      mutate(
        model = 'hurdle', 
        prior = 'default',
        explanatory_var = 'ptsd', 
        variables = 'controls'),
    hurdle_weakly_ptsd_bivariate %>% tidy_draws() %>% 
      mutate(
        model = 'hurdle', 
        prior = 'weakly_informative',
        explanatory_var = 'ptsd', 
        variables = 'bivariate'),
    hurdle_weakly_ptsd_adjustment %>% tidy_draws() %>% 
      mutate(
        model = 'hurdle', 
        prior = 'weakly_informative',
        explanatory_var = 'ptsd', 
        variables = 'adjustment_set'),
    hurdle_weakly_ptsd_controls %>% tidy_draws() %>% 
      mutate(
        model = 'hurdle', 
        prior = 'weakly_informative',
        explanatory_var = 'ptsd', 
        variables = 'controls'),
    hurdle_strongly_ptsd_bivariate %>% tidy_draws() %>% 
      mutate(
        model = 'hurdle', 
        prior = 'strongly_informative',
        explanatory_var = 'ptsd', 
        variables = 'bivariate'),
    hurdle_strongly_ptsd_adjustment %>% tidy_draws() %>% 
      mutate(
        model = 'hurdle', 
        prior = 'strongly_informative',
        explanatory_var = 'ptsd', 
        variables = 'adjustment_set'),
    hurdle_strongly_ptsd_controls %>% tidy_draws() %>% 
      mutate(
        model = 'hurdle', 
        prior = 'strongly_informative',
        explanatory_var = 'ptsd', 
        variables = 'controls'),
    
  ## Gaussian Models
    gaussian_default_ptsd_bivariate %>% tidy_draws() %>% 
      mutate(
        model = 'gaussian', 
        prior = 'default',
        explanatory_var = 'ptsd', 
        variables = 'bivariate'),
    gaussian_default_ptsd_adjustment %>% tidy_draws() %>% 
      mutate(
        model = 'gaussian', 
        prior = 'default',
        explanatory_var = 'ptsd', 
        variables = 'adjustment_set'),
    gaussian_default_ptsd_controls %>% tidy_draws() %>% 
      mutate(
        model = 'gaussian', 
        prior = 'default',
        explanatory_var = 'ptsd', 
        variables = 'controls'),
    gaussian_weakly_ptsd_bivariate %>% tidy_draws() %>% 
      mutate(
        model = 'gaussian', 
        prior = 'weakly_informative',
        explanatory_var = 'ptsd', 
        variables = 'bivariate'),
    gaussian_weakly_ptsd_adjustment %>% tidy_draws() %>% 
      mutate(
        model = 'gaussian', 
        prior = 'weakly_informative',
        explanatory_var = 'ptsd', 
        variables = 'adjustment_set'),
    gaussian_weakly_ptsd_controls %>% tidy_draws() %>% 
      mutate(
        model = 'gaussian', 
        prior = 'weakly_informative',
        explanatory_var = 'ptsd', 
        variables = 'controls'),
    gaussian_strongly_ptsd_bivariate %>% tidy_draws() %>% 
      mutate(
        model = 'gaussian', 
        prior = 'strongly_informative',
        explanatory_var = 'ptsd', 
        variables = 'bivariate'),
    gaussian_strongly_ptsd_adjustment %>% tidy_draws() %>% 
      mutate(
        model = 'gaussian', 
        prior = 'strongly_informative',
        explanatory_var = 'ptsd', 
        variables = 'adjustment_set'),
    gaussian_strongly_ptsd_controls %>% tidy_draws() %>% 
      mutate(
        model = 'gaussian', 
        prior = 'strongly_informative',
        explanatory_var = 'ptsd', 
        variables = 'controls'),
  
  
  ## Ordered Logit Models
  ordered_default_ptsd_bivariate %>% tidy_draws() %>% 
    mutate(
      model = 'ordered', 
      prior = 'default',
      explanatory_var = 'ptsd', 
      variables = 'bivariate'),
  ordered_default_ptsd_adjustment %>% tidy_draws() %>% 
    mutate(
      model = 'ordered', 
      prior = 'default',
      explanatory_var = 'ptsd', 
      variables = 'adjustment_set'),
  ordered_default_ptsd_controls %>% tidy_draws() %>% 
    mutate(
      model = 'ordered', 
      prior = 'default',
      explanatory_var = 'ptsd', 
      variables = 'controls'),
  ordered_weakly_ptsd_bivariate %>% tidy_draws() %>% 
    mutate(
      model = 'ordered', 
      prior = 'weakly_informative',
      explanatory_var = 'ptsd', 
      variables = 'bivariate'),
  ordered_weakly_ptsd_adjustment %>% tidy_draws() %>% 
    mutate(
      model = 'ordered', 
      prior = 'weakly_informative',
      explanatory_var = 'ptsd', 
      variables = 'adjustment_set'),
  ordered_weakly_ptsd_controls %>% tidy_draws() %>% 
    mutate(
      model = 'ordered', 
      prior = 'weakly_informative',
      explanatory_var = 'ptsd', 
      variables = 'controls'),
  ordered_strongly_ptsd_bivariate %>% tidy_draws() %>% 
    mutate(
      model = 'ordered', 
      prior = 'strongly_informative',
      explanatory_var = 'ptsd', 
      variables = 'bivariate'),
  ordered_strongly_ptsd_adjustment %>% tidy_draws() %>% 
    mutate(
      model = 'ordered', 
      prior = 'strongly_informative',
      explanatory_var = 'ptsd', 
      variables = 'adjustment_set'),
  ordered_strongly_ptsd_controls %>% tidy_draws() %>% 
    mutate(
      model = 'ordered', 
      prior = 'strongly_informative',
      explanatory_var = 'ptsd', 
      variables = 'controls')
  )

loo_comp_hurdle <-
  loo_compare(
    loo(hurdle_default_ptsd_bivariate),
    loo(hurdle_default_ptsd_adjustment),
    loo(hurdle_default_ptsd_controls),
    loo(hurdle_weakly_ptsd_bivariate),
    loo(hurdle_weakly_ptsd_adjustment),
    loo(hurdle_weakly_ptsd_controls),
    loo(hurdle_strongly_ptsd_bivariate),
    loo(hurdle_strongly_ptsd_adjustment),
    loo(hurdle_strongly_ptsd_controls))

loo_comp_gaussian <-
  loo_compare(
    loo(gaussian_default_ptsd_bivariate),
    loo(gaussian_default_ptsd_adjustment),
    loo(gaussian_default_ptsd_controls),
    loo(gaussian_weakly_ptsd_bivariate),
    loo(gaussian_weakly_ptsd_adjustment),
    loo(gaussian_weakly_ptsd_controls),
    loo(gaussian_strongly_ptsd_bivariate),
    loo(gaussian_strongly_ptsd_adjustment),
    loo(gaussian_strongly_ptsd_controls))
  
loo_comp_ordered <-
  loo_compare(
    loo(ordered_default_ptsd_bivariate),
    loo(ordered_default_ptsd_adjustment),
    loo(ordered_default_ptsd_controls),
    loo(ordered_weakly_ptsd_bivariate),
    loo(ordered_weakly_ptsd_adjustment),
    loo(ordered_weakly_ptsd_controls),
    loo(ordered_strongly_ptsd_bivariate),
    loo(ordered_strongly_ptsd_adjustment),
    loo(ordered_strongly_ptsd_controls)
  )

loo_comp_gaussian
loo_comp_hurdle
loo_comp_ordered


## Compare priors
results %>% 
  ggplot(aes(b_pcl_total, fill = prior)) +
  geom_density(alpha = .25) +
  facet_grid(vars(explanatory_var, model, variables)) +
  lims(x = c(0, 1))

## Compare Controls
results %>% 
  ggplot(aes(b_pcl_total, fill = variables)) +
  geom_density(alpha = .25) +
  facet_grid(vars(explanatory_var, model, prior)) +
  lims(x = c(0, 1))

## Compare Controls
results %>% 
  ggplot(aes(b_pcl_total, fill = variables)) +
  geom_density(alpha = .25) +
  facet_grid(vars(explanatory_var, model, prior)) +
  lims(x = c(0, 1))


## Compare Models
results %>% 
  ggplot(aes(b_pcl_total, fill = model)) +
  geom_density(alpha = .25) +
  facet_grid(vars(explanatory_var, prior, variables)) +
  lims(x = c(0, 1))



