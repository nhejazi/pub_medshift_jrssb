# simulate simple data for simple simulation
sim_mediation_data <- function(n_obs = 1000,        # no. observations
                               n_w = 3,             # no. baseline covariates
                               est_type = c("ipsi", "mtp"),
                               sim_type = c("param", "h0_test")) {

  # set default to avoid breaking legacy simulation code
  est_type <- match.arg(est_type)
  sim_type <- match.arg(sim_type)

  # baseline covariate -- simple, binary
  W_1 <- rbinom(n_obs, 1, prob = 0.50)
  W_2 <- rbinom(n_obs, 1, prob = 0.65)
  W_3 <- rbinom(n_obs, 1, prob = 0.35)
  W <- cbind(W_1, W_2, W_3)

  # create treatment based on baseline W: binary if IPSI, continuous if MTP
  if (est_type == "ipsi") {
    A <- as.numeric(rbinom(n_obs, 1, prob = (rowSums(W) / 4 + 0.1)))
  } else if (est_type == "mtp") {
    A <- as.numeric(rpois(n_obs, lambda = (2 * rowSums(W) + 1)))
  }

  # mediators to affect the outcome
  ## 1st mediator (binary)
  z1_prob <- 1 - plogis((A^2 + W[, 1]) / (A + W[, 1]^3 + 0.5))
  Z_1 <- rbinom(n_obs, 1, prob = z1_prob)
  ## 2nd mediator (binary)
  z2_prob <- plogis((A - 1)^3 + W[, 2] / (W[, 3] + 3))
  Z_2 <- rbinom(n_obs, 1, prob = z2_prob)
  ## 3rd mediator (binary)
  z3_prob <- plogis((A - 1)^2 + 2 * W[, 1]^3 - 1 / (2 * W[, 1] + 0.5))
  Z_3 <- rbinom(n_obs, 1, prob = z3_prob)
  ## build matrix of mediators
  Z <- cbind(Z_1, Z_2, Z_3)

  # create outcome as a linear function of A, W + white noise
  if (sim_type == "param") {
    Y <- Z_1 + Z_2 - Z_3 + A - 0.1 * rowSums(W)^2 + rnorm(n_obs, 0, 0.5)
  } else if (sim_type == "h0_test") {
    # remove direct effect for uniform test
    Y <- Z_1 + Z_2 - Z_3 - (0.5 * rowSums(W)^2) + rnorm(n_obs, 0, 1)
  }

  # output
  return(list(W, A, Z, Y))
}
