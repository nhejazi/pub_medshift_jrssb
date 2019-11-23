library(here)
library(data.table)
source(here("R", "01_setup_data.R"))

# get truth under simple simulation
get_sim_truth <- function(n_obs = 1e7,    # number of observations
                          n_w = 3,        # number of baseline covariates
                          p_w = 0.5,      # probability of success in baseline
                          delta = 0.5,    # value of shift parameter
                          est_type = c("ipsi", "mtp")) {

  # compute large data set for true values
  data <- sim_mediation_data(n_obs = n_obs,
                             n_w = n_w,
                             est_type = est_type)
  W <- data[[1]]
  Z <- as.data.table(data[[3]])
  Y <- data[[4]]

  if (est_type == "ipsi") {
    # compute TRUE G under counterfactual regimes
    g_Ais1 <- rowSums(W) / 4 + 0.1
    g_Ais0 <- 1 - g_Ais1

    # compute TRUE SHIFTED G under counterfactual regimes
    g_shifted_Ais1 <- (delta * g_Ais1) / (delta * g_Ais1 + g_Ais0)
    g_shifted_Ais0 <- 1 - g_shifted_Ais1

  } else if (est_type == "mtp") {
    # natural density
    g_natural <- dpois(x = 0:100, lambda = (2 * rowSums(W) + 1))

    # shifted density just moves location
    g_shifted <- dpois(x = 0:1, lambda = (2 * rowSums(W) + 1 + delta))
  }

  # compute TRUE M under counterfactual regimes
  m_Ais1 <- Z$Z_1 + Z$Z_2 - Z$Z_3 + 1 - 0.1 * rowSums(W)^2
  m_Ais0 <- Z$Z_1 + Z$Z_2 - Z$Z_3 + 0 - 0.1 * rowSums(W)^2

  # output: true values of nuisance parameters
  if (est_type == "ipsi") {
    return(list(g_obs_true = data.table(A1 = g_Ais1,
                                        A0 = g_Ais0),
                g_shifted_true = data.table(A1 = g_shifted_Ais1,
                                            A0 = g_shifted_Ais0),
                m_true = data.table(A1 = m_Ais1,
                                    A0 = m_Ais0),
                EY_true = mean(Y)
               )
          )
  } else if (est_type == "mtp") {
    #return()
  }
}

# simulate data and extract components for computing true parameter value
sim_truth <- get_sim_truth(est_type = "ipsi")
m_A1 <- sim_truth$m_true$A1
m_A0 <- sim_truth$m_true$A0
g_shifted_A1 <- sim_truth$g_shifted_true$A1
g_shifted_A0 <- sim_truth$g_shifted_true$A0
EY <- sim_truth$EY_true

# compute true parameter value based on the substitution estimator
true_param <- mean(m_A1 * g_shifted_A1) + mean(m_A0 * g_shifted_A0)

# compute the natural direct effect (NDE) as the difference of Y and parameter
nde_true <- EY - true_param

# clean up
rm(list = setdiff(ls(), "nde_true"))
