# to get the z score tables for the state simulations

# setup ####

## packages and settings ####

library(statnet)
library(nattswap)
library(ggplot2)

library(showtext)
font_add("STIX", "P:/pCloud Sync/Projects/school/QP/state_sims/STIX2Text-Regular.otf")
showtext_auto() 

# show text does not work well in Rstudio viewer
windows(width = 13.889, height = 2.639)

## in/out ####

t1_pol = readRDS("./data/sims/t1/pol/t1_sim_results_Politician.rda")

t1_pol_sig = get_att_swap_sig(t1_pol, "Politician")










# make plots ####

nattswap_compare(t1_sim_sig, c("all"), "lep", font_family = "STIX")




nattswap_compare(t1_psim, c("degree", "evc", "closeness", "n_bet"), "Politician", font_family = "STIX", yaxis = c(0, 1250))
nattswap_compare(t1_lsim, c("degree", "evc", "closeness", "n_bet"), "Law.Enforcement", font_family = "STIX", yaxis = c(0, 1250))

nattswap_compare(t2_lsim, c("degree", "evc", "closeness", "n_bet"), "Law.Enforcement", font_family = "STIX", yaxis = c(0, 2500))
nattswap_compare(t2_psim, c("degree", "evc", "closeness", "n_bet"), "Politician", font_family = "STIX", yaxis = c(0, 2500))
