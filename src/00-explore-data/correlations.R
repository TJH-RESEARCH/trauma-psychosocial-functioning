
library(brms)
library(modelr)
library(patchwork)
library(gganimate)
library(gifski)

# -------------------------------------------------------------------------
#"We could take a slice through these lines at an x position in the above chart (say, mios_scale = 0) and look at the correlation between them using a scatterplot matrix:" #http://mjskay.github.io/tidybayes/articles/tidy-brms.html
tibble(mios_scaled = 0) %>%
  add_epred_draws(model_bi, value = "P(bipf_category | mios_scaled)", category = "bipf_category") %>%
  ungroup() %>%
  select(.draw, bipf_category, `P(bipf_category | mios_scaled)`) %>%
  gather_pairs(bipf_category, `P(bipf_category | mios_scaled)`, triangle = "both") %>%
  filter(.row != .col) %>%
  ggplot(aes(.x, .y)) +
  geom_point(alpha = 1/50) +
  facet_grid(.row ~ .col) +
  ylab("P(bipf_category = row | mios_scaled = 0)") +
  xlab("P(bipf_category = col | mios_scaled = 0)")

