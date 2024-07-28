


x <-
tibble(
  p = seq(0, 1, .05)
) %>% 
  mutate(
  odds = p / (1 - p),
  log_odds = log(odds)
  ) %>% print(n = 21)

x %>% 
  ggplot(aes(log_odds, p)) +
  geom_point()


data %>% 
  count(bipf_category) %>% 
  mutate(p = n / sum(n),
         cum_p = cumsum(p),
         cum_log_oods = log(cum_p / (1 - cum_p))
      ) %>% 
  ggplot(aes(cum_log_oods, cum_p)) +
  geom_point() +
  lims(y = c(0,1), x = c(-4,4)) +
  geom_segment(x = -4,    y = .451,  xend = -.195, yend = .451, linetype = 2, color = 'red') +
  geom_segment(x = -4,    y =  .617, xend = .475,  yend = .617, linetype = 2, color = 'red') +
  geom_segment(x = -4,    y =  .767, xend = 1.19,  yend = .767, linetype = 2, color = 'red') +
  geom_segment(x = -4,    y =  .956, xend = 3.09,  yend = .956, linetype = 2, color = 'red') +
  geom_segment(x = -.195, y =  0 ,   xend = -.195, yend = .451, linetype = 2, color = 'blue') +
  geom_segment(x = .475,  y =  0,    xend = .475,  yend = .617, linetype = 2, color = 'blue') +
  geom_segment(x = 1.19,  y =  0,    xend = 1.19,  yend = .767, linetype = 2, color = 'blue') +
  geom_segment(x = 3.09,  y =  0,    xend = 3.09,  yend = .956, linetype = 2, color = 'blue') +
  theme_classic() +
  labs(x = 'Cumulative Log Odds', 
       y = 'Cumulative Probability')
  

draws_ord_log_multivariate %>% tidybayes::summarise_draws()

4.15/7.37 

# A change of 1 unit of moral injury symptoms (which is scaled to have
# a mean of 0 and SD = .5) is equal to a change of 2 standard deviations
# two standard deviations is therefore associated with a .53 probability 
# increase in being in the next highest category of difficulty functioning

plogis(0.133)
exp(0.133) / (1 + exp(0.133))
mean(data_scaled$mios_scaled)
sd(data_scaled$mios_scaled)
