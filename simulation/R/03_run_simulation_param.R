# use custom package library
.libPaths("/global/scratch/nhejazi/R")

# packages
library(here)
library(future)
library(future.apply)
library(data.table)
library(tidyverse)
library(sl3)
library(tmle3)
library(hal9001)
library(medshift)

# load scripts, parallelization, PRNG
source(here("R", "01_setup_data.R"))
source(here("R", "02_fit_estimators.R"))
seed_int <- 7491
plan(multiprocess, workers = 24)

# sample sizes with root-n spacing: n = {100,...,6400}
n_obs <- cumsum(rep(sqrt(100), 8))^2

# simulation parameters
n_sim <- 1000                         # number of simulations
delta_shift <- 0.5                    # value of shift parameter
est_type <- "ipsi"                    # incremental propensity score

# sequential loop over different sample sizes
sim_results <- lapply(seq_along(n_obs), function(iter) {
    # parallelized loop over iterations in the simulation
    results <- future_lapply(X = seq_len(n_sim), FUN = function(x) {
        data_sim <- sim_mediation_data(n_obs = n_obs[iter],
                                       est_type = est_type,
                                       sim_type = "param")
        out <- fit_estimators(W = data_sim[[1]],
                              A = data_sim[[2]],
                              Z = data_sim[[3]],
                              Y = data_sim[[4]],
                              delta = delta_shift)
        out[, n_samp := n_obs[iter]]
        return(out)
    }, future.seed = seed_int)

    # save in list before moving to next sample size
    results <- rbindlist(results)
    return(results)
})

# save results to file
names(sim_results) <- paste("n", n_obs, sep = "_")
timestamp <- stringr::str_replace_all(Sys.time(), " ", "_")
saveRDS(object = sim_results,
        file = here("data", paste0("medshift_binary_param_", timestamp,
                                   "_jrssb.rds")))
