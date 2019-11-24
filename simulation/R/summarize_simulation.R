library(here)
library(data.table)
library(ggthemes)
library(grid)
library(tidyverse)
library(latex2exp)
library(ggrepel)
library(ggsci)
library(patchwork)
pd <- position_dodge(0.3)
label_maker <- c(
  "Substitution (G & M w/ intercept)" = "Misspecified Parametric",
  "Substitution (G & M w/ HAL)" = "Data-Adaptive Estimation (HAL)",
  "Reweighted (G & E w/ intercept)" = "Misspecified Parametric",
  "Reweighted (G & E w/ HAL)" = "Data-Adaptive Estimation (HAL)",
  "Efficient (G, E, M w/ intercept)" = "Misspecified Parametric",
  "Efficient (G, E, M w/ HAL)" = "Data-Adaptive Estimation (HAL)",
  "TMLE (G, E, M w/ intercept)" = "Misspecified Parametric",
  "TMLE (G, E, M w/ HAL)" = "Data-Adaptive Estimation (HAL)"
)

# results and truth for parameter estimation simulation
source(here("R", "get_truth.R"))
sim_results_file <- "medshift_binary_2018-12-15_00:28:26_manuscript.rds"
sim_results <- readRDS(here("data", sim_results_file))
n_sim <- nrow(sim_results[[1]])
sim_sizes <- names(sim_results)

# results for simulation of test of no direct effect
h0test_sim_results_file <-
  "medshift_binary_testh0_2019-06-19_22:54:27_jrssb.rds"
h0sim_results <- readRDS(here("data", h0test_sim_results_file))

# load and summarize simulation results
sim_results_reshaped <-
  lapply(sim_results, function(x) {
           sim_reshaped <- melt(x, id = ncol(sim_results[[1]]),
                                variable.name = "estimator",
                                value.name = "est")
         })
ordered_nsamp <- str_replace(names(sim_results_reshaped), "_", " = ")

# get true NDE and scale by root-n
true_param <- nde_true
true_param_rootn <- nde_true *
  as.numeric(str_remove(names(sim_results_reshaped), "n_"))

sim_results_summarized <-
  rbindlist(sim_results_reshaped) %>%
  setnames(c("n", "estimator", "est")) %>%
  mutate(
    estimator = str_remove(estimator, "theta_"),
    est_type = case_when(estimator == "sub_misspec" ~
                           "Misspecified Parametric",
                         estimator == "sub_hal" ~
                           "Data-Adaptive Estimation (HAL)",
                          estimator == "re_misspec" ~
                           "Misspecified Parametric",
                          estimator == "re_hal" ~
                           "Data-Adaptive Estimation (HAL)",
                          estimator == "eff_all_misspec" ~
                           "Misspecified Parametric",
                          estimator == "eff_g_misspec" ~
                           "Misspecified Parametric",
                          estimator == "eff_e_misspec_robust" ~
                           "Misspecified Parametric",
                          estimator == "eff_m_misspec_robust" ~
                           "Misspecified Parametric",
                          estimator == "eff_gem_hal" ~
                           "Data-Adaptive Estimation (HAL)",
                          estimator == "tmle_all_misspec" ~
                           "Misspecified Parametric",
                          estimator == "tmle_g_misspec" ~
                           "Misspecified Parametric",
                          estimator == "tmle_e_misspec" ~
                           "Misspecified Parametric",
                          estimator == "tmle_m_misspec" ~
                           "Misspecified Parametric",
                          estimator == "tmle_gem_hal" ~
                           "Data-Adaptive Estimation (HAL)"),
    estimator = case_when(estimator == "sub_misspec" ~
                            "Substitution (G & M w/ intercept)",
                          estimator == "sub_hal" ~
                            "Substitution (G & M w/ HAL)",
                          estimator == "re_misspec" ~
                            "Reweighted (G & E w/ intercept)",
                          estimator == "re_hal" ~
                            "Reweighted (G & E w/ HAL)",
                          estimator == "eff_all_misspec" ~
                            "Efficient (G, E, M w/ intercept)",
                          estimator == "eff_g_misspec" ~
                            "Efficient (G w/ intercept; E, M w/ HAL)",
                          estimator == "eff_e_misspec_robust" ~
                            "Efficient (E w/ intercept; G, M w/ HAL)",
                          estimator == "eff_m_misspec_robust" ~
                            "Efficient (M w/ intercept; E, G, w/ HAL)",
                          estimator == "eff_gem_hal" ~
                            "Efficient (G, E, M w/ HAL)",
                          estimator == "tmle_all_misspec" ~
                            "TMLE (G, E, M w/ intercept)",
                          estimator == "tmle_g_misspec" ~
                            "TMLE (G w/ intercept; E, M w/ HAL)",
                          estimator == "tmle_e_misspec" ~
                            "TMLE (E w/ intercept; G, M w/ HAL)",
                          estimator == "tmle_m_misspec" ~
                            "TMLE (M w/ intercept; E, G, w/ HAL)",
                          estimator == "tmle_gem_hal" ~
                            "TMLE (G, E, M w/ HAL)"),
    n_label = factor(paste("n", as.character(n), sep = " = "),
                     levels = ordered_nsamp)
  ) %>%
  group_by(estimator, est_type, n)

###############################################################################
## HISTOGRAM COMPARING PERFORMANCE ACROSS ALL ESTIMATORS
###############################################################################
p_hist_all_est <- sim_results_summarized %>%
  mutate(
    mean_est = mean(est, na.rm = TRUE)
  ) %>%
  ungroup(n) %>%
  ggplot(aes(x = est, fill = estimator)) +
    geom_histogram(alpha = 0.75, binwidth = 0.01) +
    xlab("") +
    ylab("") +
    facet_wrap(facets = c(vars(estimator), vars(n_label)),
               ncol = length(ordered_nsamp)) +
    geom_vline(aes(xintercept = mean_est), linetype = "dotted",
               colour = "black") +
    geom_vline(aes(xintercept = true_param), linetype = "twodash",
               colour = "black") +
    ggtitle(paste("Sampling distributions of estimators of the natural direct",
                  "effect under an incremental propensity score intervention",
                  paste0("\n(", n_sim), "simulations each;",
                  length(ordered_nsamp), "sample sizes)")) +
    theme_bw() +
    theme(legend.position = "bottom")

ggsave(filename = here("graphs", "dist_comparison_estim.pdf"),
       plot = p_hist_all_est)
ggsave(filename = here("graphs", "dist_comparison_estim.jpg"),
       plot = p_hist_all_est)


###############################################################################
## SUMMARY TABLE OF ESTIMATOR PERFORMANCE
###############################################################################
table_summary_sim <- sim_results_summarized %>%
  summarise(
    bias_avg_est = abs(mean(est, na.rm = TRUE) - true_param),
    mc_var_est = var(est, na.rm = TRUE),
    mse_est = (bias_avg_est)^2 + mc_var_est
  ) %>%
  mutate(
    bias_avg_est_rootn = bias_avg_est * sqrt(n),
    mc_se_rootn = sqrt(mc_var_est * n),
    mse_est_n = sqrt(mse_est * n)
  )

knitr::kable(table_summary_sim, format = "markdown")

###############################################################################
# SUMMARY PLOT COMPARING MEAN-SQUARED ERROR (SCALED BY ROOT-N) OF ESTIMATORS
###############################################################################
p_mse_sim <- table_summary_sim %>%
  ggplot(aes(x = factor(n), y = mse_est_n, group = estimator)) +
    #geom_label_repel(aes(label = as.character(round(mse_est_rootn, 4)))) +
    geom_point(aes(colour = estimator), alpha = 0.75, size = 7, position = pd) +
    geom_line(aes(colour = estimator), linetype = "dotted", position = pd) +
    xlab("Sample Size") +
    ylab(TeX("$\\sqrt{n} \\times$ MSE")) +
    geom_hline(aes(yintercept = 0), linetype = "dashed", colour = "black") +
    ggtitle(paste("Scaled mean-squared error of estimators of the natural",
                  "direct effect under an\nincremental propensity score",
                  "intervention",
                  paste0("(", n_sim), "simulations each;",
                  length(ordered_nsamp), "sample sizes)")) +
    theme_bw() +
    theme(legend.background = element_rect(fill = "gray90", size = 0.25,
                                           linetype = "dotted"),
          legend.position = "bottom",
          legend.title = element_text(colour = "red"),
          text = element_text(size = 30),
          axis.text.x = element_text(size = 30, angle = 30, hjust = 1),
          axis.text.y = element_text(size = 30)
         ) +
    facet_grid(vars(est_type), scales = "free_y") +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))

ggsave(filename = here("graphs", "mse_scaled_comparison_estim_acic2019.pdf"),
       plot = p_mse_sim)
ggsave(filename = here("graphs", "mse_scaled_comparison_estim_acic2019.jpg"),
       plot = p_mse_sim)

###############################################################################
## SUMMARY PLOT COMPARING BIAS (SCALED BY ROOT-N) OF ESTIMATORS
###############################################################################
p_scaled_bias_sim <- table_summary_sim %>%
  ggplot(aes(x = factor(n), y = bias_avg_est_rootn, group = estimator)) +
    #geom_label_repel(aes(label = as.character(round(bias_avg_est_rootn, 4)))) +
    geom_point(aes(colour = estimator), alpha = 0.75, size = 7, position = pd) +
    geom_line(aes(colour = estimator), linetype = "dotted", position = pd) +
    xlab("Sample Size") +
    ylab(TeX("$\\sqrt{n} \\times$ |$\\psi - \\hat{\\psi}$|")) +
    geom_hline(aes(yintercept = 0), linetype = "dashed",
               colour = "black") +
    ggtitle(paste("Scaled bias of estimators of the natural direct",
                  "effect under an incremental propensity score intervention",
                  paste0("\n(", n_sim), "simulations each;",
                  length(ordered_nsamp), "sample sizes)")) +
    theme_bw() +
    theme(legend.background = element_rect(fill = "gray90", size = 0.25,
                                           linetype = "dotted"),
          legend.position = "bottom",
          legend.title = element_text(colour = "red"),
          text = element_text(size = 28),
          axis.text.x = element_text(size = 28, angle = 30, hjust = 1),
          axis.text.y = element_text(size = 28)
         ) +
    facet_grid(vars(est_type), scales = "free_y") +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))

ggsave(filename = here("graphs", "bias_scaled_comparison_estim_acic2019.pdf"),
       plot = p_scaled_bias_sim)
ggsave(filename = here("graphs", "bias_scaled_comparison_estim_acic2019.jpg"),
       plot = p_scaled_bias_sim)

###############################################################################
## SUMMARY PLOT COMPARING BIAS OF ESTIMATORS
###############################################################################
p_bias_sim <- table_summary_sim %>%
  ggplot(aes(x = factor(n), y = bias_avg_est, colour = estimator)) +
    #geom_label_repel(aes(label = as.character(round(bias_avg_est, 4)))) +
    geom_point(alpha = 0.5, size = 5) +
    xlab("Sample Size") +
    ylab(TeX("|$\\psi - \\hat{\\psi}$|")) +
    geom_hline(aes(yintercept = 0), linetype = "dashed",
               colour = "black") +
    ggtitle(paste("Bias of estimators of the natural direct",
                  "effect under an incremental propensity score intervention",
                  paste0("\n(", n_sim), "simulations each;",
                  length(ordered_nsamp), "sample sizes)")) +
    theme_bw() +
    theme(legend.background = element_rect(fill = "gray90", size = 0.25,
                                           linetype = "dotted"),
          legend.position = "bottom",
          legend.title = element_text(colour = "red"),
          text = element_text(size = 20),
          axis.text.x = element_text(angle = 30, hjust = 1)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))

ggsave(filename = here("graphs", "bias_comparison_estim.pdf"),
       plot = p_bias_sim, width = 22, height = 14)
ggsave(filename = here("graphs", "bias_comparison_estim.jpg"),
       plot = p_bias_sim, width = 22, height = 14)

###############################################################################
## SUMMARY PLOT COMPARING VARIANCE OF ESTIMATORS
###############################################################################
p_var_sim <- table_summary_sim %>%
  ggplot(aes(x = factor(n), y = mc_var_est, colour = estimator)) +
    #geom_label_repel(aes(label = as.character(round(mc_var_est, 6)))) +
    geom_point(alpha = 0.5, size = 5) +
    xlab("Sample Size") +
    ylab(TeX("$\\hat{\\sigma}^2$")) +
    geom_hline(aes(yintercept = 0), linetype = "dashed",
               colour = "black") +
    ggtitle(paste("Monte carlo variance of estimators of the natural direct",
                  "effect under an incremental propensity score intervention",
                  paste0("\n(", n_sim), "simulations each;",
                  length(ordered_nsamp), "sample sizes)")) +
    theme_bw() +
    theme(legend.background = element_rect(fill = "gray90", size = 0.25,
                                           linetype = "dotted"),
          legend.position = "bottom",
          legend.title = element_text(colour = "red"),
          text = element_text(size = 20),
          axis.text.x = element_text(angle = 30, hjust = 1)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1)))

ggsave(filename = here("graphs", "var_comparison_estim.pdf"),
       plot = p_var_sim, width = 22, height = 14)
ggsave(filename = here("graphs", "var_comparison_estim.jpg"),
       plot = p_var_sim, width = 22, height = 14)

###############################################################################
## HISTOGRAM COMPARING PERFORMANCE OF SELECT ESTIMATORS FOR MANUSCRIPT
###############################################################################
p_hist_sim_manuscript <- sim_results_summarized %>%
  mutate(
    mean_est = mean(est, na.rm = TRUE)
  ) %>%
  ungroup(n) %>%
  dplyr::filter(n > 100) %>%
  dplyr::filter(!stringr::str_detect(estimator, "Misspecified")) %>%
  ggplot(aes(x = est, fill = estimator)) +
    geom_histogram(alpha = 0.75, binwidth = 0.01) +
    xlab("") +
    ylab("") +
    facet_wrap(facets = c(vars(estimator), vars(n_label)),
               ncol = (length(ordered_nsamp) - 1), nrow = 6) +
    geom_vline(aes(xintercept = mean_est), linetype = "dotted",
               colour = "black") +
    geom_vline(aes(xintercept = true_param), linetype = "twodash",
               colour = "black") +
    theme_bw() +
    theme(legend.position = "bottom")

ggsave(filename = here("graphs", "dist_comparison_estim_manuscript.pdf"),
       plot = p_hist_sim_manuscript, width = 22, height = 14)
ggsave(filename = here("graphs", "dist_comparison_estim_manuscript.jpg"),
       plot = p_hist_sim_manuscript, width = 22, height = 14)

###############################################################################
## REDUCED SUMMARY TABLE OF ESTIMATOR PERFORMANCE FOR MANUSCRIPT
###############################################################################
sim_results_manuscript <- sim_results_summarized %>%
  ungroup(estimator) %>%
  dplyr::filter(n > 100) %>%
  group_by(estimator, n) %>%
  summarise(
    bias_avg_est = abs(mean(est, na.rm = TRUE) - true_param),
    mc_var_est = var(est, na.rm = TRUE),
    mse_est = (bias_avg_est)^2 + mc_var_est
  ) %>%
  mutate(
    bias_avg_est_rootn = bias_avg_est * sqrt(n),
    mc_se_rootn = sqrt(mc_var_est * n),
    mse_est_n = mse_est * n
  )

table_sim_manuscript <- sim_results_manuscript %>%
  transmute(
    N = n,
    MSE_n = round(mse_est_n, 3)  # multiply by 100 for readability
  ) %>%
  dplyr::filter(!stringr::str_detect(estimator, "Misspecified")) %>%
  as.data.table() %>%
  dcast(estimator ~ N) %>%
  setnames(., c("Estimator",
                paste("MSE", stringr::str_remove(sim_sizes[-1], "n_"),
                                                 sep = "_")))

knitr::kable(table_sim_manuscript[c(5, 6, 3, 1, 4, 2), ], format = "latex")

# labeler to be used for simplifying facet headers with facet_wrap
simplify_est_labels <- c(
  "Efficient (E w/ intercept; G, M w/ HAL)" = "Efficient Emis",
  "Efficient (G w/ intercept; E, M w/ HAL)" = "Efficient Gmis",
  "Efficient (G, E, M w/ HAL)" = "Efficient",
  "Efficient (M w/ intercept; E, G, w/ HAL)" = "Efficient Mmis",
  "Efficient (G, E, M w/ intercept)" = "Efficient (all mis.)",
  "Reweighted (G & E w/ intercept)" = "Mis. Reweighted (IPW)",
  "Substitution (G & M w/ intercept)" = "Mis. Substitution",
  "Reweighted (G & E w/ HAL)" = "Reweighted",
  "Substitution (G & M w/ HAL)" = "Substitution"
)

###############################################################################
# SUMMARY PLOT COMPARING MEAN-SQUARED ERROR (SCALED BY ROOT-N) FOR MANUSCRIPT
###############################################################################
p_mse_sim <- sim_results_manuscript %>%
  dplyr::filter(!stringr::str_detect(estimator, "Misspecified")) %>%
  ggplot(aes(x = factor(n), y = mse_est_n, group = estimator)) +
    #geom_label_repel(aes(label = as.character(round(mse_est_rootn, 4)))) +
    geom_point(alpha = 0.5, size = 7) +
    geom_line(linetype = "dotted") +
    xlab("") +
    ylab(TeX("$n \\times$ MSE")) +
    geom_hline(aes(yintercept = 0), linetype = "dashed",
               colour = "black") +
    #ggtitle(paste("Scaled mean-squared error of estimators of the",
                  #"natural direct effect under an incremental propensity score",
                  #"intervention",
                  #paste0("\n(", n_sim), "simulations each;",
                  #length(ordered_nsamp), "sample sizes)")) +
    scale_color_lancet() +
    theme_bw() +
    theme(legend.background = element_rect(fill = "gray90", size = 0.25,
                                           linetype = "dotted"),
          legend.position = "none",
          legend.title = element_text(colour = "red"),
          text = element_text(size = 26),
          axis.text.x = element_text(angle = 40, hjust = 1)) +
    #guides(colour = guide_legend(override.aes = list(alpha = 0.5))) +
    facet_wrap(vars(estimator), nrow = 6, ncol = 1, scales = "free_y",
               labeller = as_labeller(simplify_est_labels, multi_line = TRUE))

ggsave(filename = here("graphs", "mse_scaled_comparison_manuscript.pdf"),
       plot = p_mse_sim, width = 22, height = 14)
ggsave(filename = here("graphs", "mse_scaled_comparison_manuscript.jpg"),
       plot = p_mse_sim, width = 22, height = 14)

###############################################################################
## SUMMARY PLOT COMPARING BIAS (SCALED BY ROOT-N) FOR MANUSCRIPT
###############################################################################
p_scaled_bias_sim <- sim_results_manuscript %>%
  dplyr::filter(!stringr::str_detect(estimator, "Misspecified")) %>%
  ggplot(aes(x = factor(n), y = bias_avg_est_rootn, group = estimator)) +
    #geom_label_repel(aes(label = as.character(round(bias_avg_est_rootn, 4)))) +
    geom_point(alpha = 0.5, size = 7) +
    geom_line(linetype = "dotted") +
    xlab("") +
    ylab(TeX("$\\sqrt{n} \\times$ |$\\hat{\\psi} - \\psi$|")) +
    geom_hline(aes(yintercept = 0), linetype = "dashed",
               colour = "black") +
    #ggtitle(paste("Scaled absolute bias of estimators of the natural direct",
                  #"effect under an incremental propensity score intervention",
                  #paste0("\n(", n_sim), "simulations each;",
                  #length(ordered_nsamp), "sample sizes)")) +
    scale_color_lancet() +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size = 26),
          axis.text.x = element_text(angle = 40, hjust = 1)) +
    facet_wrap(vars(estimator), nrow = 6, ncol = 1, scales = "free_y",
               labeller = as_labeller(simplify_est_labels, multi_line = TRUE))

ggsave(filename = here("graphs", "bias_scaled_comparison_manuscript.pdf"),
       plot = p_scaled_bias_sim, width = 22, height = 14)
ggsave(filename = here("graphs", "bias_scaled_comparison_manuscript.jpg"),
       plot = p_scaled_bias_sim, width = 22, height = 14)

###############################################################################
## SUMMARY PLOT COMPARING BIAS FOR MANUSCRIPT
###############################################################################
p_bias_sim <- sim_results_manuscript %>%
  dplyr::filter(!stringr::str_detect(estimator, "Misspecified")) %>%
  ggplot(aes(x = factor(n), y = bias_avg_est, group = estimator)) +
    #geom_label_repel(aes(label = as.character(round(bias_avg_est, 4)))) +
    geom_point(alpha = 0.5, size = 7) +
    geom_line(linetype = "dotted") +
    xlab("") +
    ylab(TeX("|$\\hat{\\psi} - \\psi$|")) +
    geom_hline(aes(yintercept = 0), linetype = "dashed",
               colour = "black") +
    #ggtitle(paste("Absolute bias of estimators of the natural direct",
                  #"effect under an incremental propensity score intervention",
                  #paste0("\n(", n_sim), "simulations each;",
                  #length(ordered_nsamp), "sample sizes)")) +
    scale_color_lancet() +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size = 26),
          axis.text.x = element_text(angle = 40, hjust = 1)) +
    facet_wrap(vars(estimator), nrow = 6, ncol = 1, scales = "free_y",
               labeller = as_labeller(simplify_est_labels, multi_line = TRUE))

ggsave(filename = here("graphs", "bias_comparison_manuscript.pdf"),
       plot = p_bias_sim, width = 22, height = 14)
ggsave(filename = here("graphs", "bias_comparison_manuscript.jpg"),
       plot = p_bias_sim, width = 22, height = 14)

###############################################################################
## SUMMARY PLOT COMPARING VARIANCE (SCALED BY ROOT-N) FOR MANUSCRIPT
###############################################################################
p_scaled_se_sim <- sim_results_manuscript %>%
  dplyr::filter(n > 100) %>%
  dplyr::filter(!stringr::str_detect(estimator, "Misspecified")) %>%
  ggplot(aes(x = factor(n), y = mc_se_rootn, group = estimator)) +
    #geom_label_repel(aes(label = as.character(round(mc_var_est, 6)))) +
    geom_point(alpha = 0.5, size = 7) +
    geom_line(linetype = "dotted") +
    xlab("") +
    ylab(TeX("$\\sqrt{n} \\times \\hat{\\sigma}$")) +
    geom_hline(aes(yintercept = 0), linetype = "dashed",
               colour = "black") +
    #ggtitle(paste("Monte carlo variance of estimators of the natural direct",
                  #"effect under an incremental propensity score intervention",
                  #paste0("\n(", n_sim), "simulations each;",
                  #length(ordered_nsamp), "sample sizes)")) +
    scale_color_lancet() +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size = 26),
          axis.text.x = element_text(angle = 40, hjust = 1)) +
    facet_wrap(vars(estimator), nrow = 6, ncol = 1, scales = "free_y",
               labeller = as_labeller(simplify_est_labels, multi_line = TRUE))

ggsave(filename = here("graphs", "var_scaled_comparison_manuscript.pdf"),
       plot = p_scaled_se_sim, width = 22, height = 14)
ggsave(filename = here("graphs", "var_scaled_comparison_manuscript.jpg"),
       plot = p_scaled_se_sim, width = 22, height = 14)

###############################################################################
## SUMMARY PLOT COMPARING VARIANCE FOR MANUSCRIPT
###############################################################################
p_var_sim <- sim_results_manuscript %>%
  dplyr::filter(n > 100) %>%
  dplyr::filter(!stringr::str_detect(estimator, "Misspecified")) %>%
  ggplot(aes(x = factor(n), y = mc_var_est, group = estimator)) +
    #geom_label_repel(aes(label = as.character(round(mc_var_est, 6)))) +
    geom_point(alpha = 0.5, size = 7) +
    geom_line(linetype = "dotted") +
    xlab("") +
    ylab(TeX("$\\hat{\\sigma}^2$")) +
    geom_hline(aes(yintercept = 0), linetype = "dashed",
               colour = "black") +
    #ggtitle(paste("Monte carlo variance of estimators of the natural direct",
                  #"effect under an incremental propensity score intervention",
                  #paste0("\n(", n_sim), "simulations each;",
                  #length(ordered_nsamp), "sample sizes)")) +
    scale_color_lancet() +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size = 26),
          axis.text.x = element_text(angle = 30, hjust = 1)) +
    facet_wrap(vars(estimator), nrow = 6, ncol = 1, scales = "free_y",
               labeller = as_labeller(simplify_est_labels, multi_line = TRUE))

ggsave(filename = here("graphs", "var_comparison_manuscript.pdf"),
       plot = p_var_sim, width = 22, height = 14)
ggsave(filename = here("graphs", "var_comparison_manuscript.jpg"),
       plot = p_var_sim, width = 22, height = 14)

###############################################################################
## COMBINED SUMMARY PLOT FOR MANUSCRIPT
###############################################################################
p_manuscript <- p_bias_sim | p_scaled_bias_sim | p_scaled_se_sim | p_mse_sim

ggsave(filename = here("graphs", "compiled_comparison_manuscript.pdf"),
       plot = p_manuscript, width = 22, height = 14)
ggsave(filename = here("graphs", "compiled_comparison_manuscript.jpg"),
       plot = p_manuscript, width = 22, height = 14)

###############################################################################
## TEST OF NO DIRECT EFFECT
###############################################################################
h0sim_results <- do.call(cbind, h0sim_results)
h0sim_summary <- apply(h0sim_results, 2, function(pvals) {
  is_sig <- (pvals < 0.05)
  return(is_sig)
})

###############################################################################
## PLOT FOR MANUSCRIPT
###############################################################################
est_labels <- c(
  "Efficient (G, E, M w/ HAL)" = "Efficient",
  "Efficient (E w/ intercept; G, M w/ HAL)" = "Eff (E-mis)",
  "Efficient (G w/ intercept; E, M w/ HAL)" = "Eff (G-mis)",
  "Efficient (M w/ intercept; E, G, w/ HAL)" = "Eff (M-mis)",
  "Efficient (G, E, M w/ intercept)" = "Eff (all-mis)",
  "Reweighted (G & E w/ intercept)" = "Mis-Reweighted",
  "Substitution (G & M w/ intercept)" = "Mis-Substitution",
  "Reweighted (G & E w/ HAL)" = "Reweighted",
  "Substitution (G & M w/ HAL)" = "Substitution"
)

sim_res <- sim_results_manuscript %>%
    select(estimator, n, bias_avg_est, bias_avg_est_rootn, mc_se_rootn,
           mse_est_n) %>%
    group_by(estimator, n) %>%
    gather(statistic, value, bias_avg_est, bias_avg_est_rootn, mc_se_rootn,
           mse_est_n) %>%
    ungroup() %>%
    mutate(estimator = factor(estimator,
                              levels = names(est_labels),
                              labels = est_labels),
           statistic = factor(statistic,
                              levels = c('bias_avg_est', 'bias_avg_est_rootn',
                                         'mc_se_rootn', 'mse_est_n'),
                              labels = c("group('|',Bias,'|')",
                                         "root(n)~group('|',Bias,'|')",
                                         'root(n)~sigma',
                                         'root(n~MSE)'))) %>%
    dplyr::filter(estimator %in% c("Efficient", "Eff (E-mis)", "Eff (M-mis)",
                                   "Eff (G-mis)", "Reweighted", "Substitution"))
ns <- unique(sim_res$n)

plot1 <-  sim_res %>%
    dplyr::filter(estimator != "Eff (G-mis)", n > 100) %>%
    ggplot(aes(n, value)) +
    ylab('') +
    geom_point(size = 2) +
    geom_line(linetype = "dotted", size = 0.5) +
    theme_hc() +
    scale_x_continuous(breaks = ns, trans = 'sqrt') +
    facet_grid(statistic ~ estimator, scales = 'free', switch = 'y',
               labeller = label_parsed) +
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 60, hjust = 1),
          text = element_text(size = 25),
          panel.spacing.x = unit(1, "lines"),
          panel.spacing.y = unit(2, "lines"))

plot2 <-  sim_res %>%
    dplyr::filter(estimator == "Eff (G-mis)", n > 100) %>%
    ggplot(aes(n, value)) +
    ylab('') +
    geom_point(size = 2) +
    geom_line(linetype = "dotted", size = 0.5) +
    theme_hc() +
    scale_x_continuous(breaks = ns, trans = 'sqrt') +
    facet_grid(statistic ~ estimator, scales = 'free', switch = 'y',
               labeller = label_parsed) +
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 60, hjust = 1),
          strip.text.y = element_blank(), text = element_text(size = 25),
          panel.spacing.y = unit(2, "lines"))

pdf(here("graphs", "plot-manuscript.pdf"), width = 15, height = 10)
grid.newpage()
grid.draw(cbind(ggplotGrob(plot1), ggplotGrob(plot2), size = 'last'))
dev.off()
