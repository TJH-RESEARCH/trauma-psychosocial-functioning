
# Save a color palette to use later
colors_tam <- MetBrewer::met.brewer("Tam")

# Color scheme for Prior Predictive Check plots in Bayesplot
bayesplot::color_scheme_set(c("grey40", colors_tam[8], colors_tam[7], colors_tam[6], colors_tam[5], colors_tam[4]))
