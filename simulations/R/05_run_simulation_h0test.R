# use custom package library
.libPaths("/global/scratch/nhejazi/R")

# packages
library(here)
library(foreach)
library(future)
library(doFuture)
library(data.table)
library(tidyverse)
library(hal9001)
library(sl3)
library(medshift)

# load scripts, parallelization, PRNG
source(here("R", "01_setup_data.R"))
source(here("R", "04_test_de.R"))
options(future.globals.maxSize = 10^12)
registerDoFuture()
plan(multiprocess, workers = 24)
seed_int <- 7491
set.seed(seed_int)

# sample sizes with root-n spacing: n = {100,...,6400}
n_obs <- cumsum(rep(sqrt(100), 7))^2

# simulation parameters
n_sim <- 5000                         # number of simulations
est_type <- "ipsi"                    # incremental propensity score

# sequential loop over different sample sizes
sim_results <- lapply(seq_along(n_obs), function(samp_iter) {
    # parallelized loop over iterations in the simulation
    results <- foreach(sim_iter = seq_len(n_sim),
                       .options.multicore = list(preschedule = FALSE),
                       .errorhandling = "remove",
                       .combine = c) %dopar% {
        # simulate data with no direct effect
        data_sim <- sim_mediation_data(n_obs = n_obs[samp_iter],
                                       est_type = est_type,
                                       sim_type = "h0_test")

        # perform hypothesis test
        out <- test_de_h0(W = data_sim[[1]],
                          A = data_sim[[2]],
                          Z = data_sim[[3]],
                          Y = data_sim[[4]],
                          v_folds = 5)
        out
    }
    return(results)
})

# save results to file
names(sim_results) <- paste("n", n_obs, sep = "_")
timestamp <- str_replace_all(Sys.time(), " ", "_")
saveRDS(object = sim_results,
        file = here("data", paste0("medshift_binary_testh0_", timestamp,
                                   "_jrssb.rds")))
