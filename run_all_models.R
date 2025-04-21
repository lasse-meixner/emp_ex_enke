# 0. Libraries & source helpers ----
library(tidyverse)

# preprocessing & data‐list
source("src/process_data.R")
# Stan‐based models (DML_B, DML_B2, R2D2)
source("src/fit_stan.R")
# BLR / FDML / IW‐BLR
source("src/fit_BLR.R")

# 1. Define the three outcomes to loop over ----
outcomes <- c("gop_2016", "trump_election_ave", "trump_primaries")

# 2. Initialize list to collect per‐outcome results ----
all_res <- vector("list", length(outcomes))
names(all_res) <- outcomes

# 3. Loop over outcomes ----
for (o in outcomes) {
  
  ## ---- Stan‐based DML variants ----
  # 3.1 LKJ half‐prior
  fit_b  <- fit_model_dml_b(o)
  res_b  <- extract_results_BDML(fit_b,   method_name = "BDML_LKJ")
  res_b$outcome <- o

  # 3.2 Inverse‐Wishart
  fit_b2 <- fit_model_dml_hp_b(o)
  res_b2 <- extract_results_BDML(fit_b2,  method_name = "BDML_HP_LKJ")
  res_b2$outcome <- o

  # 3.3 Simple LKJ (“R2D2”)
  fit_r2 <- fit_model_dml_hp_iw(o)
  res_r2 <- extract_results_BDML(fit_r2,  method_name = "BDML_HP_IW")
  res_r2$outcome <- o

  ## ---- BLR‐based methods ----
  # 3.4 BLR variants (naive, Hahn, Linero)
  fits_blr <- fit_model_BLR(o)
  res_blr  <- extract_results_BLR(fits_blr)
  res_blr$outcome <- o

  # 3.5 FDML (full & split)
  fits_fdml <- fit_model_FDML(o)
  res_fdml  <- extract_results_FDML(fits_fdml)
  res_fdml$outcome <- o

  # 3.6 IW‐BLR
  alpha_iw   <- fit_mvn_iw_model(o)
  res_iw     <- extract_results_IW(alpha_iw)
  res_iw$outcome <- o

  # 3.7 Bind this outcome’s results ----
  all_res[[o]] <- bind_rows(
    res_b, res_b2, res_r2,
    res_blr,
    res_fdml,
    res_iw
  )
}

# 4. Combine all outcomes and write to CSV ----
final_results <- bind_rows(all_res)
write_csv(final_results, "results/results.csv")