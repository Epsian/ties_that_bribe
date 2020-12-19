# to run attribute swap simulations on the lep attribute

# setup ####

## packages and settings ####

library(statnet)
library(nattswap)
library(parallel)

# how many cores to use in parallel
.cores = 30

# what attribute do you want to simulate?
.sim_att = "lep"

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
.t1_sim_results = paste0("./data/sims/t1_sim_results_", .sim_att, ".rda")
.t2_sim_results = paste0("./data/sims/t2_sim_results_", .sim_att, ".rda")

# where to save sig lists
.t1_sim_sig_out = paste0("./data/sims/t1/", .sim_att, "/t1_sim_sigs_", .sim_att, ".rda")
.t2_sim_sig_out = paste0("./data/sims/t2/", .sim_att, "/t2_sim_sigs_", .sim_att, ".rda")

# where to save simulation output

# data load ####

t1.l.g = readRDS(.t1_net_loc)
t2.l.g = readRDS(.t2_net_loc)

# run simulations ####

# LEP combined
t1_sims = run_att_swap_sims(net_to_swap = t1.l.g, attribute = .sim_att, swaps = 10000, save_dir = paste0(.t1_sim_dir, "/", .sim_att), sims = 10000, save_preface = paste0("t1_sim_", .sim_att), verbose = FALSE, cores = .cores)
t2_sims = run_att_swap_sims(net_to_swap = t2.l.g, attribute = .sim_att, swaps = 10000, save_dir = paste0(.t2_sim_dir, "/", .sim_att), sims = 10000, save_preface = paste0("t2_sim_", .sim_att), verbose = FALSE, cores = .cores)

# reformat sims ####

t1_combined_sims = combine_net_att_sims(t1_sims, swapped_attribute = .sim_att)
t2_combined_sims = combine_net_att_sims(t2_sims, swapped_attribute = .sim_att)

# get plot format ####

t1_sim_sig = get_att_swap_sig(t1_combined_sims, .sim_att)
t2_sim_sig = get_att_swap_sig(t2_combined_sims, .sim_att)

# save out ####

saveRDS(t1_sim_sig, .t1_sim_sig_out)
saveRDS(t2_sim_sig, .t2_sim_sig_out)


