# PMH_Survival
Simple simulation about the impact predictor measurement heterogeneity in time-to-event outcomes.


# Aim
The goal of the simulation is to illustrate the impact of predictor measurement heterogeneity on measures of predictive performance of time-to-event prediction models in large sample simulations (n = 100000). We generated a single continuous predictor under different scenarios of predictor measurement heterogeneity and generated survival times from an exponential distribution in three forms: witout censoring, with administrative censoring and with administrative and random censoring. We evaluated predictie performance at median follow-up time in terms of the time-dependent cumulative c-statistic, the Index of prediction accuracy and calibration-in-the-large coefficient and calibration slope from a Poisson recalibration model.

# Simulation structure

The structure of this simulation is based on the R package targets by William Michael Landau. See https://docs.ropensci.org/targets/ for more information.

├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── targets. R                    <- make file to execute project
├── targets                       <- storage of simulation targets
├── data                          <- processed simulation output (.rds files)
├── plot                          <- final results of simulation
└── R                             <- source code for this project
    ├── analysis                  <- helper script to perform model derivation and validation
    ├── dgm                       <- helper script that defines the data-generating-mechanisms for the simulation
    ├── scenarios                 <- helper script that defines the scenarios of predictor measurement heterogeneity evaluated in the simulation
    ├── target_structure_additive <- helper script to structure targets for targets.R file
    ├── target_structure_multiplicative <- helper script to structure targets for targets.R file
    └── target_structure_random   <- helper script to structure targets for targets.R file



# Description of functions

TO BE ADDED
