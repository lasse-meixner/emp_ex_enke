# install.packages(c("haven","fixest","modelsummary"))
library(haven)       # for read_dta()
library(fixest)      # for feols()
library(modelsummary) # for nice tables (optional)
library(tidyverse)


## load data
survey <- read_dta("data/Survey.dta")

# 2. Pre‐processing: factor / numeric recoding + scaling
survey <- survey %>%
  mutate(
    # --- fixed effects factors ---
    county    = factor(county),
    yob       = factor(yob),
    ethnicity = factor(ethnicity),
    occupation = factor(occupation),
    religious_denomination = factor(religious_denomination),

    # --- binary flags as numeric ---
    female   = as.numeric(female),
    employed = as.numeric(employed),

    # --- standardize into z‐scores: all explanatory vars ---
    values_relative    = as.numeric(scale(values_relative)),
    liberalism         = as.numeric(scale(liberalism)),
    religiosity        = as.numeric(scale(religiosity)),
    abs_values_relative = as.numeric(scale(abs_values_relative)),
    altruism           = as.numeric(scale(altruism)),
    trust_general      = as.numeric(scale(trust_general))
    # note: std_ln_inc, std_educ, std_ln_pd are already z‐scored in the .dta
  )

## run regressions

# 3. Build RHS to match:
#    global benchmark        = "liberalism std_ln_inc std_educ std_ln_pd religiosity"
#    global controls_benchmark = "female i.yob i.ethnicity employed i.occupation i.religious_denomination abs_values_relative altruism trust_general"
benchmark       <- c("values_relative", "liberalism", "std_ln_inc", "std_educ", "std_ln_pd", "religiosity")
controls_bench  <- c("female", "yob", "ethnicity", "employed",
                     "occupation", "religious_denomination",
                     "abs_values_relative", "altruism", "trust_general")
rhs <- paste(c(benchmark, controls_bench), collapse = " + ")

# 4. Estimate the three OLS with county FE & robust SEs
m1 <- feols(
  as.formula(paste("gop_2016 ~", rhs, "| county")),
  data = survey,
  vcov = "hetero"
)

m2 <- feols(
  as.formula(paste("trump_election_ave ~", rhs, "| county")),
  data = survey,
  vcov = "hetero"
)

m3 <- feols(
  as.formula(paste("trump_primaries ~", rhs, "| county")),
  data = survey,
  vcov = "hetero"
)

# (optional) quick check
summary(m1)
summary(m2)
summary(m3)
