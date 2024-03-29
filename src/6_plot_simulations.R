# to get the z score tables for the state simulations

# setup ####

## packages and settings ####

library(statnet)
library(nattswap)
library(ggplot2)

library(showtext)
font_add("STIX", "./docs/STIX2Text-Regular.otf")
showtext_auto() 

# show text does not work well in Rstudio viewer
windows(width = 13.889, height = 2.639)

## in/out ####

t1_pol = readRDS("./data/sims/t1/pol/t1_sim_sigs_Politician.rda")
t1_le = readRDS("./data/sims/t1/le/t1_sim_sigs_Law_Enforcement.rda")

t2_pol = readRDS("./data/sims/t2/pol/t2_sim_sigs_Politician.rda")
t2_le = readRDS("./data/sims/t2/le/t2_sim_sigs_Law_Enforcement.rda")

# make x axis settings list ####
t1_xaxis_settings = list(
  "degree" = c(2, 16),
  "evc" = c(0, .1),
  "nestedness" = c(1.5,7)
)

t2_xaxis_settings = list(
  "degree" = c(2.5, 25),
  "evc" = c(0, .05),
  "nestedness" = c(1,8)
)

# make plots ####

pdf(file="./vis/histogram/t1_pol.pdf", height = 2.639, width = 13.889)
nattswap_compare(t1_pol, c("degree", "evc", "nestedness"), "Politician", font_family = "STIX", yaxis = c(0, 1250), xaxis = t1_xaxis_settings, bin_num = 70)
dev.off()

pdf(file="./vis/histogram/t1_le.pdf", width = 13.889, height = 2.639)
nattswap_compare(t1_le, c("degree", "evc", "nestedness"), "Law.Enforcement", font_family = "STIX", yaxis = c(0, 1250), xaxis = t1_xaxis_settings, bin_num = 70)
dev.off()

pdf(file="./vis/histogram/t2_pol.pdf", width = 13.889, height = 2.639)
nattswap_compare(t2_pol, c("degree", "evc", "nestedness"), "Politician", font_family = "STIX", yaxis = c(0, 1250), xaxis = t2_xaxis_settings, bin_num = 70)
dev.off()

pdf(file="./vis/histogram/t2_le.pdf", width = 13.889, height = 2.639)
nattswap_compare(t2_le, c("degree", "evc", "nestedness"), "Law.Enforcement", font_family = "STIX", yaxis = c(0, 1250), xaxis = t2_xaxis_settings, bin_num = 70)
dev.off()

