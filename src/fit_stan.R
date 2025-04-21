## src/fit_stan.R ----

# 1. Load packages ----
library(cmdstanr)
library(tidyverse)
source("src/process_data.R")


# 2. Compile the Stan models ----
model_dml_hp_b    <- cmdstan_model("BDML_HP_LKJ.stan")  # hierarchical LKJ
model_dml_hp_iw   <- cmdstan_model("BDML_HP_IW.stan")   # Inv‐Wishart
model_dml_b <- cmdstan_model("BDML_LKJ.stan")     # simple LKJ

# 3. Fit model HP_B  ----
#    Expects data: J, N, x, y
fit_model_dml_hp_b <- function(outcome_var) {
  dat <- create_data_list(outcome_var)
  fit <- model_dml_hp_b$sample(
    data             = dat,
    chains           = 4,
    parallel_chains  = 4,
    refresh          = 500,
    show_messages    = TRUE,
    show_exceptions  = TRUE
  )
  return(fit)
}

# 4. Fit model HP_IW (Inverse‐Wishart) ----
#    Expects data: J, N, x, y
fit_model_dml_hp_iw <- function(outcome_var) {
  dat <- create_data_list(outcome_var)
  fit <- model_dml_hp_iw$sample(
    data             = dat,
    chains           = 4,
    parallel_chains  = 4,
    refresh          = 500,
    show_messages    = TRUE,
    show_exceptions  = TRUE
  )
  return(fit)
}

# 5. Fit (simple LKJ) ----
#    Expects data: K, J, N, x, y
fit_model_dml_b <- function(outcome_var) {
  dat <- create_data_list(outcome_var)
  dat$K <- 2      # two outcomes: [Y, D]
  fit <- model_dml_b$sample(
    data             = dat,
    chains           = 4,
    parallel_chains  = 4,
    refresh          = 500,
    show_messages    = TRUE,
    show_exceptions  = TRUE
  )
  return(fit)
}

# 6. Extract α from any fitted model ----
extract_results_BDML <- function(fit, method_name) {
  draws <- fit$draws("alpha")
  m     <- mean(draws)
  ci    <- quantile(draws, c(0.025, 0.975))
  data.frame(
    method         = method_name,
    gamma_hat      = m,
    LCL            = ci[1],
    UCL            = ci[2],
    interval_width = ci[2] - ci[1]
  )
}

# Example:
# fit <- fit_model_dml_b("gop_2016")
# res <- extract_results_BDML(fit, "LKJ‐half")
# print(res)