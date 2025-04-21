## src/fit_BLR.R ----

# 0. Libraries ----
library(tidyverse)
library(BLR)          # for BLR()
library(bayesm) # for multivariate Inverse Wishart model
source("src/process_data.R")  # defines create_data_list(outcome_var)

# 1. BLR‐based estimators ----
#    Naive, Hahn & Linero
fit_model_BLR <- function(outcome_var) {
  dl <- create_data_list(outcome_var)
  Y  <- dl$y[,1]
  D  <- dl$y[,2]
  X  <- dl$x

  # suppress BLR output
  invisible(capture.output({
    naive   <- BLR(y = Y,    XR = X,        XF = matrix(D, ncol=1))
    ps_fit  <- BLR(y = D,    XR = X)
    hahn    <- BLR(y = Y,    XR = X, XF = matrix(D - ps_fit$yHat, ncol=1))
    linero  <- BLR(y = Y,    XR = X, XF = cbind(D, ps_fit$yHat))
  }))

  list(naive = naive, hahn = hahn, linero = linero)
}

extract_results_BLR <- function(fit_list) {
  # fit_list should be one of naive, hahn or linero from fit_model_BLR()
  results <- imap_dfr(fit_list, function(fit, name) {
    gamma_hat     <- fit$bF[1]
    se            <- fit$SD.bF[1]
    interval      <- gamma_hat + c(-1.96, 1.96) * se
    data.frame(
      method         = name,
      gamma_hat      = gamma_hat,
      LCL            = interval[1],
      UCL            = interval[2],
      interval_width = diff(interval)
    )
  })
  return(results)
}

# 2. FDML (full‐data and split‐sample) ----
fit_model_FDML <- function(outcome_var) {
  dl <- create_data_list(outcome_var)
  Y  <- dl$y[,1]
  D  <- dl$y[,2]
  X  <- dl$x

  # 2.1 full‐sample DML
  ps_full <- BLR(y = D, XR = X)
  y_full  <- BLR(y = Y, XR = X)
  a_res   <- D - ps_full$mu - X %*% ps_full$bR
  y_res   <- Y - y_full$mu - X %*% y_full$bR
  dml_full <- lm(y_res ~ a_res)

  # 2.2 split‐sample DML
  n  <- nrow(X)
  set.seed(1999)
  ix <- sample.int(n)
  half <- floor(n/2)
  i1 <- ix[1:half]; i2 <- ix[(half+1):n]

  ps1 <- BLR(y = D[i1], XR = X[i1, , drop=FALSE])
  y1  <- BLR(y = Y[i1], XR = X[i1, , drop=FALSE])
  a_res <- D[i2] - ps1$mu - X[i2, , drop=FALSE] %*% ps1$bR
  y_res <- Y[i2] - y1$mu  - X[i2, , drop=FALSE] %*% y1$bR
  dml_split <- lm(y_res ~ a_res)

  list(FDML_full  = dml_full,
       FDML_split = dml_split)
}

extract_results_FDML <- function(fit_list) {
  imap_dfr(fit_list, function(fit, name) {
    coef_tab   <- summary(fit)$coefficients
    gamma_hat  <- coef_tab["a_res","Estimate"]
    CI         <- confint(fit)["a_res",]
    data.frame(
      method         = name,
      gamma_hat      = gamma_hat,
      LCL            = CI[1],
      UCL            = CI[2],
      interval_width = diff(CI)
    )
  })
}

# 3. BLR‐based Inverse‐Wishart (IW) Bayes ----
#    “fit_mvn_iw_model” returns a vector of posterior draws for alpha
fit_mvn_iw_model <- function(outcome_var) {
  dl <- create_data_list(outcome_var)
  Y  <- dl$y[,1]
  D  <- dl$y[,2]
  X  <- dl$x

  # two‐equation SUR setup: equation 1 for Y, eq 2 for D
  reg_data <- list(
    list(y = Y, X = X),
    list(y = D, X = X)
  )

  invisible(capture.output({
    mcmc <- rsurGibbs(
      Data = list(regdata = reg_data),
      Prior = list(
        betabar = rep(0, ncol(X)*2),
        A       = diag(1/ncol(X), ncol(X)*2),
        nu      = 4,
        V       = diag(1, 2)
      ),
      Mcmc = list(R = 1000, keep = 1, nprint = 0)
    )
  }))

  # extract alpha = Sigma[1,2]/Sigma[2,2] from each draw
  Sig_draws  <- mcmc$Sigmadraw   # (1000 × 4 matrix: columns [11,12,21,22] flattening)
  alpha_draws <- Sig_draws[,2] / Sig_draws[,4]
  return(alpha_draws)
}

extract_results_IW <- function(alpha_draws) {
  m <- mean(alpha_draws)
  ci <- quantile(alpha_draws, c(0.025, 0.975))
  data.frame(
    method         = "IW‐BLR",
    gamma_hat      = m,
    LCL            = ci[1],
    UCL            = ci[2],
    interval_width = diff(ci)
  )
}

# ---- Example workflow ----
# brs <- fit_model_BLR("gop_2016")
# extract_results_BLR(brs)
#
# fd <- fit_model_FDML("gop_2016")
# extract_results_FDML(fd)
#
# iw_draws <- fit_mvn_iw_model("gop_2016")
# extract_results_IW(iw_draws)
