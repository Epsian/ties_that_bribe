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

t1_pol = readRDS("./data/sims/t1/pol/t1_sim_sigs_Politician.rda")
t1_le = readRDS("./data/sims/t1/le/t1_sim_sigs_Law_Enforcement.rda")

t2_pol = readRDS("./data/sims/t2/pol/t2_sim_sigs_Politician.rda")
t2_le = readRDS("./data/sims/t2/le/t2_sim_sigs_Law_Enforcement.rda")

# make plots ####

tiff(file="./vis/histogram/t1_pol.tiff", width = 13.889 * 200, height = 2.639 * 200, units = "px", pointsize = 20, res = 200, family = "STIX", compression = "none")
nattswap_compare(t1_pol, c("degree", "evc", "nestedness"), "Politician", font_family = "STIX", yaxis = c(0, 1250))
dev.off()

tiff(file="./vis/histogram/t1_le.tiff", width = 13.889 * 200, height = 2.639 * 200, units = "px", pointsize = 20, res = 200, family = "STIX", compression = "none")
nattswap_compare(t1_le, c("degree", "evc", "nestedness"), "Law.Enforcement", font_family = "STIX", yaxis = c(0, 1250))
dev.off()

tiff(file="./vis/histogram/t2_pol.tiff", width = 13.889 * 200, height = 2.639 * 200, units = "px", pointsize = 20, res = 200, family = "STIX", compression = "none")
nattswap_compare(t2_pol, c("degree", "evc", "nestedness"), "Politician", font_family = "STIX", yaxis = c(0, 1250))
dev.off()

tiff(file="./vis/histogram/t2_le.tiff", width = 13.889 * 200, height = 2.639 * 200, units = "px", pointsize = 20, res = 200, family = "STIX", compression = "none")
nattswap_compare(t2_le, c("degree", "evc", "nestedness"), "Law.Enforcement", font_family = "STIX", yaxis = c(0, 1250))
dev.off()
