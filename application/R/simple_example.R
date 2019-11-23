# packages
library(here)
library(tidyverse)  # pandoc can't deal with the startup message
library(data.table)
library(SuperLearner)
library(sl3)
library(medshift)
library(npcausal)
library(mma)
library(future)

# options: parallelization, prng, etc
options(scipen = 999)  ## has scientific notation ever annoyed you?
sl3_debug_mode()
plan(multiprocess)
set.seed(429153)

################################################################################
## RUN EXAMPLE REMOTELY
################################################################################

# convenience function to compute inference via delta method: EY1 - EY0
param_linear_contrast <- function(params, eifs, ci_level = 0.95) {
  # bounds for confidence interval
  ci_norm_bounds <- c(-1, 1) * abs(stats::qnorm(p = (1 - ci_level) / 2))
  param_est <- params[[1]] - params[[2]]
  eif <- eifs[[1]] - eifs[[2]]
  se_eif <- sqrt(var(eif) / length(eif))
  param_ci <- param_est + ci_norm_bounds * se_eif
  # parameter and inference
  out <- c(param_ci[1], param_est, param_ci[2])
  names(out) <- c("lwr_ci", "param_est", "upr_ci")
  return(out)
}

data(weight_behavior)
dim(weight_behavior)
head(weight_behavior)

# remove missing values
is_na <- unique(do.call(c, apply(apply(weight_behavior, 2, is.na), 2, which)))
weight_behavior_complete <- weight_behavior[-is_na, ]
weight_behavior_complete$sports <-
  as.numeric(weight_behavior_complete$sports) - 1
dim(weight_behavior_complete)
head(weight_behavior_complete)

Y <- weight_behavior_complete$bmi
A <- weight_behavior_complete$sports
Z <- weight_behavior_complete %>%
  select(snack, exercises, overweigh)
W <- weight_behavior_complete %>%
  select(age, sex, race, numpeople, car, gotosch, tvhours, cmpthours,
         cellhours, sweat)

delta_shift_ipsi <- 2

# random forest learner based on ranger
mean_lrnr <- Lrnr_mean$new()
rf_lrnr_50trees <- Lrnr_ranger$new(num.trees = 50)
rf_lrnr_100trees <- Lrnr_ranger$new(num.trees = 100)
rf_lrnr_500trees <- Lrnr_ranger$new(num.trees = 500)

# SL learners used for continuous data (the nuisance parameter M)
fglm_contin_lrnr <- Lrnr_glm_fast$new(family = gaussian())
ridge_contin_lrnr <- Lrnr_glmnet$new(alpha = 0, family = "gaussian")
enet_contin_lrnr <- Lrnr_glmnet$new(alpha = 0.5, family = "gaussian")
lasso_contin_lrnr <- Lrnr_glmnet$new(alpha = 1, family = "gaussian")
xgboost_lrnr_50_contin <- Lrnr_xgboost$new(nrounds = 50,
                                           objective = "reg:linear")
xgboost_lrnr_100_contin <- Lrnr_xgboost$new(nrounds = 100,
                                            objective = "reg:linear")
xgboost_lrnr_300_contin <- Lrnr_xgboost$new(nrounds = 300,
                                            objective = "reg:linear")
hal_deg3_contin <- Lrnr_hal9001$new(n_folds = 5, degrees = 3,
                                    fit_type = "glmnet")
hal_deg5_contin <- Lrnr_hal9001$new(n_folds = 5, degrees = 5,
                                    fit_type = "glmnet")
contin_lrnr_lib <- Stack$new(mean_lrnr,
                             fglm_contin_lrnr,
                             ridge_contin_lrnr,
                             enet_contin_lrnr,
                             lasso_contin_lrnr,
                             hal_deg3_contin,
                             hal_deg5_contin,
                             xgboost_lrnr_50_contin,
                             xgboost_lrnr_100_contin,
                             xgboost_lrnr_300_contin,
                             rf_lrnr_50trees,
                             rf_lrnr_100trees,
                             rf_lrnr_500trees)
sl_contin_lrnr <- Lrnr_sl$new(learners = contin_lrnr_lib,
                              metalearner = Lrnr_nnls$new())

# SL learners used for binary data (nuisance parameters G and E in this case)
fglm_binary_lrnr <- Lrnr_glm_fast$new(family = binomial())
ridge_binary_lrnr <- Lrnr_glmnet$new(alpha = 0, family = "binomial")
enet_binary_lrnr <- Lrnr_glmnet$new(alpha = 0.5, family = "binomial")
lasso_binary_lrnr <- Lrnr_glmnet$new(alpha = 1, family = "binomial")
xgboost_lrnr_50_binary <- Lrnr_xgboost$new(nrounds = 50,
                                           objective = "reg:logistic")
xgboost_lrnr_100_binary <- Lrnr_xgboost$new(nrounds = 100,
                                            objective = "reg:logistic")
xgboost_lrnr_300_binary <- Lrnr_xgboost$new(nrounds = 300,
                                            objective = "reg:logistic")
hal_deg3_binary <- Lrnr_hal9001$new(n_folds = 5, degrees = 3,
                                    fit_type = "glmnet", family = "binomial")
hal_deg5_binary <- Lrnr_hal9001$new(n_folds = 5, degrees = 5,
                                    fit_type = "glmnet", family = "binomial")
binary_lrnr_lib <- Stack$new(mean_lrnr,
                             fglm_binary_lrnr,
                             ridge_binary_lrnr,
                             enet_binary_lrnr,
                             lasso_binary_lrnr,
                             hal_deg3_binary,
                             hal_deg5_binary,
                             xgboost_lrnr_50_binary,
                             xgboost_lrnr_100_binary,
                             xgboost_lrnr_300_binary,
                             rf_lrnr_50trees,
                             rf_lrnr_100trees,
                             rf_lrnr_500trees)
sl_binary_lrnr <- Lrnr_sl$new(learners = binary_lrnr_lib,
                              metalearner = Lrnr_nnls$new())

# let's compute the parameter where both A and Z are shifted
psi_ipsi_fit <- ipsi(y = Y, a = A, x.trt = W, x.out = W,
                     time = rep(1, length(Y)), id = seq_along(Y),
                     delta.seq = delta_shift_ipsi, nsplits = 10,
                     progress_bar = FALSE, return_ifvals = TRUE)

# print return object and extract point estimate
psi_ipsi_param <- psi_ipsi_fit$res$est
psi_ipsi_ci <- c(psi_ipsi_fit$res$ci.ll, psi_ipsi_fit$res$ci.ul)
psi_ipsi_eif <- as.numeric(psi_ipsi_fit$ifvals)
psi_ipsi_fit$res


# let's compute the parameter where A (but not Z) are shifted
medshift_start <- proc.time()
theta_eff <- medshift(W = W, A = A, Z = Z, Y = Y,
                      delta = delta_shift_ipsi,
                      g = sl_binary_lrnr,
                      e = sl_binary_lrnr,
                      m = sl_contin_lrnr,
                      phi = fglm_contin_lrnr,
                      estimator = "onestep",
                      estimator_args = list(cv_folds = 10))
medshift_end <- proc.time()
theta_eff
(medshift_time <- medshift_end - medshift_start)

# parameter estimates and EIFs for components of direct effect
EY <- mean(Y)
eif_EY <- Y - EY
params_nde <- list(EY, theta_eff$theta)
eifs_nde <- list(eif_EY, theta_eff$eif)

# natural direct effect = EY - estimated quantity
nde_est <- param_linear_contrast(params_nde, eifs_nde)
nde_est


# parameter estimates and EIFs for components of indirect effect
params_nie <- list(theta_eff$theta, psi_ipsi_param)
eifs_nie <- list(theta_eff$eif, psi_ipsi_eif)

# natural indirect effect = estimated quantity - Edward's estimate
nie_est <- param_linear_contrast(params_nie, eifs_nie)
nie_est


# save output
objects_save <- list(nde_estim = nde_est, nie_estim = nie_est,
                     nde_param = params_nde, nde_eif = eifs_nde,
                     nie_param = params_nie, nie_eif = eifs_nie,
                     medshift = theta_eff, ipsi = psi_ipsi_fit,
                     EY = EY, EY_eif = eif_EY)
saveRDS(objects_save,
        file = here("data", "simple_example_results.rds"))

################################################################################
## ASSESS EXAMPLE RESULTS LOCALLY
################################################################################
results_data <- readRDS(here("data", "simple_example_results.rds"))

# natural direct effect
nde_results <- (results_data$nde_estim)
nde_var <- var(results_data$nde_eif[[1]] - results_data$nde_eif[[2]]) /
  length(results_data$EY_eif)

# natural indirect effect
nie_results <- (results_data$nie_estim)
nie_var <- var(results_data$nie_eif[[1]] - results_data$nie_eif[[2]]) /
  length(results_data$EY_eif)

nde_results
nie_results
