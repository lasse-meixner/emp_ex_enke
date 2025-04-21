# process_data.R

# 0. Packages
library(haven)
library(dplyr)
library(tidyr)

# 1. Read & initial preprocess
survey <- read_dta("data/Survey.dta") %>%
  mutate(
    # — factor‐fixed effects —
    county    = factor(county),
    yob       = factor(yob),
    ethnicity = factor(ethnicity),
    occupation = factor(occupation),
    religious_denomination = factor(religious_denomination),

    # — binary flags numeric —
    female   = as.numeric(female),
    employed = as.numeric(employed),

    # — standardized (z‐score) continuous vars —
    values_relative     = as.numeric(scale(values_relative)),
    liberalism          = as.numeric(scale(liberalism)),
    religiosity         = as.numeric(scale(religiosity)),
    abs_values_relative = as.numeric(scale(abs_values_relative)),
    altruism            = as.numeric(scale(altruism)),
    trust_general       = as.numeric(scale(trust_general))
    # note: std_ln_inc, std_educ, std_ln_pd are already standardized
  )

# 2. Define exactly the RHS covariates
benchmark        <- c("liberalism", "std_ln_inc", "std_educ", "std_ln_pd", "religiosity") # without D = values_relative
controls_bench   <- c("female", "yob", "ethnicity", "employed",
                      "occupation", "religious_denomination",
                      "abs_values_relative", "altruism", "trust_general")



# 3. Stan data‐list constructor
#    - outcome_var: one of "gop_2016", "trump_election_ave", "trump_primaries"
#    - returns list(N, J, x, y) with y[,1]=Y, y[,2]=D
create_data_list <- function(outcome_var) {
  if (!outcome_var %in% names(survey)) {
    stop("Unknown outcome: ", outcome_var)
  }

  # extract raw vectors
  Y <- survey[[outcome_var]]
  D <- survey[["values_relative"]]

  # build X_mat
  X_df <- survey %>%
    select(all_of(c(benchmark, controls_bench, "county")))
  # 1) Build a model frame that keeps all rows
  mf <- model.frame(~ . - 1, 
                    data      = X_df,
                    na.action = na.pass)

  # 2) Then extract the design matrix
  X_mat <- model.matrix(~ . - 1, data = mf)

  # bind into one big matrix so complete.cases() works
  M <- cbind(Y = Y, D = D, X_mat)

  cc <- complete.cases(M)      # a logical vector of length nrow(M)
  M_cc <- M[cc, , drop = FALSE] 

  # re‐extract post‐drop
  Y_cc <- M_cc[, "Y"]
  D_cc <- M_cc[, "D"]
  X_cc <- M_cc[, setdiff(colnames(M_cc), c("Y","D")), drop = FALSE]

  N <- nrow(X_cc)
  J <- ncol(X_cc)

  # Stan wants y as an N×2 matrix (or array[N] vector[2])
  y_mat <- cbind(Y_cc, D_cc)

  list(
    N = N,
    J = J,
    x = X_cc,
    y = y_mat
  )
}