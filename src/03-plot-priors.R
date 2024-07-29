
plot_priors <- 
  tibble(
    beta_prior = rcauchy(n = 1e5, location = 0, scale = 2.5),
    alpha_prior = rt(n = 1e5, df = 3, ncp = 2.5),
    id = rep(1, 1e5)
  )  %>% 
    pivot_longer(-id) %>% 
    ggplot(aes(value)) +
    geom_density(aes(fill = name), alpha = .25) +
    lims(x = c(-15, 15))

plot_priors %>% print()
