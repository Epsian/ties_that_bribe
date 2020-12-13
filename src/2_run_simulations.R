# to run attribute swap simulations on the lep attribute

# setup ####

## packages and settings ####

library(statnet)
library(nattswap)
library(parallel)

## in/out ####

# location of largest component networks
## t1 network
.t1_net_loc = "./data/t1.l.g.rda"
## t2 network
.t2_net_loc = "./data/t2.l.g.rda"

# where to save network simulations
.t1_sim_dir = "./data/sims/t1"
.t2_sim_dir = "./data/sims/t2"

# where to save combined simulations results
.t1_sim_results = "./data/sims/t1_sim_results.rda"
.t2_sim_results = "./data/sims/t2_sim_results.rda"

# where to save simulation output

# data load ####

t1.l.g = readRDS(.t1_net_loc)
t2.l.g = readRDS(.t2_net_loc)

# run simulations ####

t1_sims = run_att_swap_sims(net_to_swap = t1.l.g, attribute = "lep", swaps = 10000, save_dir = .t1_sim_dir, sims = 10, save_preface = "t1_sim", verbose = FALSE)
t2_sims = run_att_swap_sims(net_to_swap = t2.l.g, attribute = "lep", swaps = 10000, save_dir = .t1_sim_dir, sims = 10, save_preface = "t2_sim", verbose = FALSE)

# reformat sims ####

t1_combined_sims = combine_net_att_sims(t1_sims, swapped_attribute = "lep")
t2_combined_sims = combine_net_att_sims(t2_sims, swapped_attribute = "lep")

# save out ####

saveRDS(t1_combined_sims, .t1_sim_results)
saveRDS(t2_combined_sims, .t2_sim_results)