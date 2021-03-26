library(targets)
library(tarchetypes)
library(tibble)

# Source functions
source( "./R/scenarios_pmh.R")
source( "./R/dgm.R")
source( "./R/analysis.R")
#source( "./R/plotting.R")


# Set target-specific options such as packages.
tar_option_set( packages = c( "timeROC",
                              "rms",
                              "scales",
                              "survival",
                              "riskRegression",
                              "ggplot2",
                              "gridExtra"))

# Call predefined target structures
source("./R/target_structure_additive.R")
source("./R/target_structure_multiplicative.R")
source("./R/target_structure_random.R")

# End this file with a list of target objects.
list(
  # systematic additive predictor measurement heterogeneity
  additive,
  combined_add_nocens,
  saved_summary_add_x_no_cens,
  saved_summary_add_w_no_cens,
  combined_add_censadmin,
  saved_summary_add_x_admin_cens,
  saved_summary_add_w_admin_cens,
  combined_add_censrandom,
  saved_summary_add_x_random_cens,
  saved_summary_add_w_random_cens,
  
  # systematic multiplicative predictor measurement heterogeneity
  multiplicative,
  combined_mult_nocens,
  saved_summary_mult_x_no_cens,
  saved_summary_mult_w_no_cens,
  combined_mult_censadmin,
  saved_summary_mult_x_admin_cens,
  saved_summary_mult_w_admin_cens,
  combined_mult_censrandom,
  saved_summary_mult_x_random_cens,
  saved_summary_mult_w_random_cens,
  
  # random predictor measurement heterogeneity
  random,
  combined_random_nocens,
  saved_summary_random_x_no_cens,
  saved_summary_random_w_no_cens,
  combined_random_censadmin,
  saved_summary_random_x_admin_cens,
  saved_summary_random_w_admin_cens,
  combined_random_censrandom,
  saved_summary_random_x_random_cens,
  saved_summary_random_w_random_cens
)


# make_figure_maintext()



### dgms are sequential (take input previous)
# Target: data, no censoring
# Target: data, administrative censoring
# Target: data, administrative + random censoring
### allen onder verschillende meetheterogeniteit
# sys additive scenarios
# sys multiplicative scenarios
# random scenarios


# Two sets of targets (simulations and sensitivity method)