library(targets)
library(tarchetypes)

# Load support functions
source( "./R/scenarios_pmh.R")
source( "./R/dgm.R")
source( "./R/analysis.R")
#source( "./simulations/R/plotting.R")

# Set target-specific options such as packages.
tar_option_set( packages = c( "tidyverse",
                              "timeROC",        # cumulative-incidence time-dependent AUC
                              "rms",
                              "scales",
                              "survival",
                              "riskRegression", # index of prediction accuracy
                              "ggplot2",
                              "gridExtra"))


# Create scenarios grid using scenarios_pmh.R
### Note: the created scenarios are those as described in the main manuscript.
### To adapt simulation parameters, modify the script in scenarios_pmh.R.

scenario_grid <- generate_sim_scenarios()

# Create 'static' branches
simulation_body <- tar_map(
  unlist = FALSE, # Important: it means object simulation_body will be a list
  values = scenario_grid, # Iterate over rows of scenario_grid
  tar_target( results,
              one_sim_scenario( n_obs = 100000,
                                psi = psi,
                                theta = theta,
                                sigma_epsilon = sigma_epsilon,
                                pmh_type = pmh_type,
                                t_val = 6.5))
)

# Build pipeline
list(
  simulation_body,
  
  # Combine all scenarios
  tar_combine(
    all_scenarios,
    simulation_body,
    command = dplyr::bind_rows(!!!.x)
  )
  # ... function to summarise all repetitions
)