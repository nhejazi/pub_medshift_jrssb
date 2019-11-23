################################################################################
# fit estimators
################################################################################
fit_estimators <- function(W, A, Z, Y, delta) {

  # instantiate learners
  hal_binary_lrnr <- Lrnr_hal9001$new(n_folds = 5, fit_type = "glmnet",
                                      family = "binomial",
                                      lambda.min.ratio = 0.00001,
                                      type.measure = "deviance")
  hal_contin_lrnr <- Lrnr_hal9001$new(n_folds = 5, fit_type = "glmnet",
                                      family = "gaussian",
                                      lambda.min.ratio = 0.00001,
                                      type.measure = "deviance")
  fglm_binary_lrnr <- Lrnr_glm_fast$new(family = binomial())
  fglm_contin_lrnr <- Lrnr_glm_fast$new(family = gaussian())
  mean_lrnr <- Lrnr_mean$new()

  # build cross-validated learners for CV-TMLE
  cv_hal_binary_lrnr <- Lrnr_cv$new(hal_binary_lrnr, full_fit = TRUE)
  cv_hal_contin_lrnr <- Lrnr_cv$new(hal_contin_lrnr, full_fit = TRUE)
  cv_fglm_binary_lrnr <- Lrnr_cv$new(fglm_binary_lrnr, full_fit = TRUE)
  cv_fglm_contin_lrnr <- Lrnr_cv$new(fglm_contin_lrnr, full_fit = TRUE)
  cv_mean_lrnr <- Lrnr_cv$new(mean_lrnr, full_fit = TRUE)

  # substitution estimator; g and m misspecified
  theta_sub_misspec <- medshift(W = W, A = A, Z = Z, Y = Y,
                                delta = delta,
                                g_learners = mean_lrnr,
                                m_learners = mean_lrnr,
                                estimator = "substitution")

  # substitution estimator; g and m fit with HAL
  theta_sub_hal <- medshift(W = W, A = A, Z = Z, Y = Y,
                            delta = delta,
                            g_learners = hal_binary_lrnr,
                            m_learners = hal_contin_lrnr,
                            estimator = "substitution")

  # re-weighted estimator; g and e misspecified
  theta_re_misspec <- medshift(W = W, A = A, Z = Z, Y = Y,
                               delta = delta,
                               g_learners = mean_lrnr,
                               e_learners = mean_lrnr,
                               estimator = "reweighted")

  # re-weighted estimator; g and e fit with HAL
  theta_re_hal <- medshift(W = W, A = A, Z = Z, Y = Y,
                           delta = delta,
                           g_learners = hal_binary_lrnr,
                           e_learners = hal_binary_lrnr,
                           estimator = "reweighted")

  # one-step estimator; all HAL
  theta_os_gem_hal <- medshift(W = W, A = A, Z = Z, Y = Y,
                               delta = delta,
                               g_learners = hal_binary_lrnr,
                               e_learners = hal_binary_lrnr,
                               m_learners = hal_contin_lrnr,
                               phi_learners = fglm_contin_lrnr,
                               estimator = "onestep")

  # one-step estimator; outcome regression misspecified
  theta_os_m_misspec <- medshift(W = W, A = A, Z = Z, Y = Y,
                                 delta = delta,
                                 g_learners = hal_binary_lrnr,
                                 e_learners = hal_binary_lrnr,
                                 m_learners = mean_lrnr,
                                 phi_learners = fglm_contin_lrnr,
                                 estimator = "onestep")

  # one-step estimator; mediator regression misspecified
  theta_os_e_misspec <- medshift(W = W, A = A, Z = Z, Y = Y,
                                 delta = delta,
                                 g_learners = hal_binary_lrnr,
                                 e_learners = mean_lrnr,
                                 m_learners = hal_contin_lrnr,
                                 phi_learners = fglm_contin_lrnr,
                                 estimator = "onestep")

  # one-step estimator; propensity score misspecified
  theta_os_g_misspec <- medshift(W = W, A = A, Z = Z, Y = Y,
                                 delta = delta,
                                 g_learners = mean_lrnr,
                                 e_learners = hal_binary_lrnr,
                                 m_learners = hal_contin_lrnr,
                                 phi_learners = fglm_contin_lrnr,
                                 estimator = "onestep")

  # one-step estimator; all nuisance parameters misspecified
  theta_os_all_misspec <- medshift(W = W, A = A, Z = Z, Y = Y,
                                   delta = delta,
                                   g_learners = mean_lrnr,
                                   e_learners = mean_lrnr,
                                   m_learners = mean_lrnr,
                                   phi_learners = fglm_contin_lrnr,
                                   estimator = "onestep")

  # CV-TMLE with ULFM; all HAL
  #theta_tmle_gem_hal <- medshift(W = W, A = A, Z = Z, Y = Y,
                                 #delta = delta,
                                 #g_learners = cv_hal_binary_lrnr,
                                 #e_learners = cv_hal_binary_lrnr,
                                 #m_learners = cv_hal_contin_lrnr,
                                 #phi_learners = cv_fglm_contin_lrnr,
                                 #estimator = "tmle",
                                 #estimator_args = list(max_iter = 1e3,
                                                       #step_size = 1e-6)
                                #)

  # CV-TMLE with ULFM; outcome regression misspecified
  #theta_tmle_m_misspec <- medshift(W = W, A = A, Z = Z, Y = Y,
                                   #delta = delta,
                                   #g_learners = cv_hal_binary_lrnr,
                                   #e_learners = cv_hal_binary_lrnr,
                                   #m_learners = cv_mean_lrnr,
                                   #phi_learners = cv_fglm_contin_lrnr,
                                   #estimator = "tmle",
                                   #estimator_args = list(max_iter = 1e3,
                                                         #step_size = 1e-6)
                                  #)

  # CV-TMLE with ULFM; mediator regression misspecified
  #theta_tmle_e_misspec <- medshift(W = W, A = A, Z = Z, Y = Y,
                                   #delta = delta,
                                   #g_learners = cv_hal_binary_lrnr,
                                   #e_learners = cv_mean_lrnr,
                                   #m_learners = cv_hal_contin_lrnr,
                                   #phi_learners = cv_fglm_contin_lrnr,
                                   #estimator = "tmle",
                                   #estimator_args = list(max_iter = 1e3,
                                                         #step_size = 1e-6)
                                  #)

  # CV-TMLE with ULFM; propensity score misspecified
  #theta_tmle_g_misspec <- medshift(W = W, A = A, Z = Z, Y = Y,
                                   #delta = delta,
                                   #g_learners = cv_mean_lrnr,
                                   #e_learners = cv_hal_binary_lrnr,
                                   #m_learners = cv_hal_contin_lrnr,
                                   #phi_learners = cv_fglm_contin_lrnr,
                                   #estimator = "tmle",
                                   #estimator_args = list(max_iter = 1e3,
                                                         #step_size = 1e-6)
                                  #)

  # CV-TMLE with ULFM; all nuisance parameters misspecified
  #theta_tmle_all_misspec <- medshift(W = W, A = A, Z = Z, Y = Y,
                                     #delta = delta,
                                     #g_learners = cv_mean_lrnr,
                                     #e_learners = cv_mean_lrnr,
                                     #m_learners = cv_mean_lrnr,
                                     #phi_learners = cv_fglm_contin_lrnr,
                                     #estimator = "tmle",
                                     #estimator_args = list(max_iter = 1e3,
                                                           #step_size = 1e-6)
                                    #)

  # vector of parameter estimates needed to compute the NDE
  param_est <- c(theta_sub_hal$theta,
                 theta_sub_misspec$theta,
                 theta_re_hal$theta,
                 theta_re_misspec$theta,
                 theta_os_gem_hal$theta,
                 theta_os_g_misspec$theta,
                 theta_os_e_misspec$theta,
                 theta_os_m_misspec$theta,
                 theta_os_all_misspec$theta)
                 #theta_tmle_gem_hal$summary$psi,
                 #theta_tmle_g_misspec$summary$psi,
                 #theta_tmle_e_misspec$summary$psi,
                 #theta_tmle_m_misspec$summary$psi,
                 #theta_tmle_all_misspec$summary$psi)

  # compute finite-sample average of outcome for the NDE
  y_avg <- mean(Y)
  nde_est <- as.data.table(t(y_avg - param_est))
  setnames(nde_est, c("theta_sub_hal",
                      "theta_sub_misspec",
                      "theta_re_hal",
                      "theta_re_misspec",
                      "theta_os_gem_hal",
                      "theta_os_g_misspec",
                      "theta_os_e_misspec",
                      "theta_os_m_misspec",
                      "theta_os_all_misspec"))
                      #"theta_tmle_gem_hal",
                      #"theta_tmle_g_misspec",
                      #"theta_tmle_e_misspec",
                      #"theta_tmle_m_misspec",
                      #"theta_tmle_all_misspec"))
  return(nde_est)
}
