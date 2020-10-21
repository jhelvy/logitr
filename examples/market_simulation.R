# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Preview the reformatted 'Yogurt' data set from the mlogit package
head(yogurt)

# ============================================================================
# Run Market Simulation Using Estimated Models

# Read in saved estimated models
mnl_pref <- readRDS(here::here('examples', 'results', 'mnl_pref.Rds'))
mnl_wtp  <- readRDS(here::here('examples', 'results', 'mnl_wtp.Rds'))
mxl_pref <- readRDS(here::here('examples', 'results', 'mxl_pref.Rds'))
mxl_wtp  <- readRDS(here::here('examples', 'results', 'mxl_wtp.Rds'))

# Create a set of alternatives for which to simulate shares. Each row is an
# alternative and each column an attribute. In this example, I just use one
# of the choice observations from the yogurt dataset:
market = subset(yogurt, obsID == 42,
              select = c('feat', 'price', 'dannon', 'hiland', 'yoplait'))
row.names(market) = c('dannon', 'hiland', 'weight', 'yoplait')
market

# Run the simulation using the preference space MNL model:
sim_mnl_pref = simulateShares(mnl_pref, market, alpha = 0.025)
sim_mnl_pref

# The results show the expected shares for each alternative.
# The low and high values show a 95% confidence interval, estimated using
# simulation. You can change the CI level by setting alpha to a different
# value (e.g. a 90% CI is obtained with alpha = 0.05).

# Run the simulation using the WTP space MNL model:
sim_mnl_wtp = simulateShares(mnl_wtp, market, priceName = 'price')
sim_mnl_wtp

# Since these two models are equivalent except in different spaces, the
# simulation results should be the same. Note that 'priceName' is the name
# of the price attribute in the market argument and must be included for
# WTP space models.

# Simulations can also be run using MXL models in either space:
sim_mxl_pref = simulateShares(mxl_pref, market)
sim_mxl_pref

sim_mxl_wtp = simulateShares(mxl_wtp, market, priceName = 'price')
sim_mxl_wtp

# Plot simulation results from preference space MNL model:
library(ggplot2)
mnl_pref_simulation$alt = row.names(mnl_pref_simulation)
ggplot(mnl_pref_simulation, aes(x = alt, y = share_mean)) +
    geom_bar(stat = 'identity', width = 0.7, fill = "dodgerblue") +
    geom_errorbar(aes(ymin = share_low, ymax = share_high), width = 0.2) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = 'Alternative', y = 'Expected Share') +
    theme_bw()

# Save results
saveRDS(sim_mnl_wtp,
        here::here('examples', 'results', 'sim_mnl_wtp.Rds'))
saveRDS(sim_mnl_pref,
        here::here('examples', 'results', 'sim_mnl_pref.Rds'))
saveRDS(sim_mxl_wtp,
        here::here('examples', 'results', 'sim_mxl_wtp.Rds'))
saveRDS(sim_mxl_wtp,
        here::here('examples', 'results', 'sim_mxl_wtp.Rds'))
