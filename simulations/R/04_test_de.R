################################################################################
# perform uniform hypothesis test of no direct effect
################################################################################
test_de_h0 <- function(W, A, Z, Y, v_folds, alpha_level = 0.05) {
  # instantiate learners
  hal_binary_lrnr <- Lrnr_hal9001$new(n_folds = 5,
                                      fit_type = "glmnet",
                                      family = "binomial",
                                      standardize = FALSE,
                                      lambda.min.ratio = 0.001)
  hal_contin_lrnr <- Lrnr_hal9001$new(n_folds = 5,
                                      fit_type = "glmnet",
                                      family = "gaussian",
                                      standardize = FALSE,
                                      lambda.min.ratio = 0.001)
  fglm_binary_lrnr <- Lrnr_glm_fast$new(family = binomial())
  fglm_contin_lrnr <- Lrnr_glm_fast$new(family = gaussian())

  # perform uniform test of direct effect, using all HAL-based learners
  test_h0 <- test_de(W = W, A = A, Z = Z, Y = Y,
                     delta_grid = seq(0.5, 4.0, 0.5),
                     mult_type = "rademacher",
                     ci_level = (1 - alpha_level),
                     cv_folds = v_folds,
                     g_learners = fglm_binary_lrnr,
                     e_learners = hal_binary_lrnr,
                     m_learners = fglm_contin_lrnr,
                     phi_learners = hal_contin_lrnr)

  # return just the p-value for the uniform test of direct effect
  return(test_h0$pval_de)
}
