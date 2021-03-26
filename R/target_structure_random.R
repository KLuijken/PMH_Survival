# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Helper file structure targets additive predictor measurement heterogeneity (pmh)
# -----------------------------------------------------------------------------#

# Define simulation scenarios of random pmh structures ----

generate_random_scenarios <- function(psi           = c( 0, 0.3, 0.6),
                                      theta         = c( 0.5, 1, 2),
                                      sigma_epsilon = c( 0.1, 0.4, 0.8, 1, 1.2, 1.7, 2)
                                      ){
  # create scenario grid
  random_scenarios <- expand.grid( psi           = psi,
                                   theta         = theta,
                                   sigma_epsilon = sigma_epsilon)
  
  # remove redundant scenarios
  random_scenarios <- subset( random_scenarios, (!( psi == max( psi) & 
                                                      theta == min( theta)) &
                                                   !( psi == min( psi) &
                                                        theta == max( theta))))
  # correction for targets naming
  random_scenarios <- random_scenarios * 10
  
  return(random_scenarios)
}

sim_scenarios_random <- generate_random_scenarios()


# Define static branches for targets ----

random <- tar_map(
  unlist = FALSE, # Return a nested list from tar_map() for combining later
  values = tibble( generate_random_scenarios()), # map over simulation scenarios
  
  # simulate data without censoring
  tar_target( random_data_no_cens, 
              generate_data_no_cens( psi = psi,
                                     theta = theta,
                                     sigma_epsilon = sigma_epsilon)),
  # analyze uncensored data
  tar_target( random_results_no_cens, 
              analyse_data( data = random_data_no_cens),
              format = "rds"),
  
  # introduce administrative censoring at t = 15
  tar_target( random_data_admin_cens, 
              generate_data_admin_cens( data_no_censoring = random_data_no_cens,
                                        c_admin = 15)),
  
  # analyze administratively censored data
  tar_target( random_results_admin_cens, 
              analyse_data( data = random_data_admin_cens),
              format = "rds"),
  
  # introduce random censoring
  tar_target( random_data_random_cens, 
              generate_data_random_cens( data_admin_censoring = random_data_admin_cens,
                                         data_no_censoring = random_data_no_cens)),
  
  # analyze randomly censored data
  tar_target( random_results_random_cens, 
              analyse_data( data = random_data_random_cens),
              format = "rds")
)

# Define combined targets (rbind all results) ----

combined_random_nocens <- tar_combine(
  combined_random_no_cens,
  random[[2]],
  command = dplyr::bind_rows(!!!.x)
)

combined_random_censadmin <- tar_combine(
  combined_random_admin_cens,
  random[[4]],
  command = dplyr::bind_rows(!!!.x)
)

combined_random_censrandom <- tar_combine(
  combined_random_random_cens,
  random[[6]],
  command = dplyr::bind_rows(!!!.x)
)

# Save output ----

saved_summary_random_x_no_cens <- tar_target( analysis_summary_random_x_no_cens, 
                                           saveRDS( cbind( sim_scenarios_random,
                                                           tar_read(combined_random_no_cens)[tar_read(combined_random_no_cens)$pred == "X",]
                                           ),
                                           file = "./data/analysis_summary_random_no_cens_x.rds")
)
saved_summary_random_w_no_cens <- tar_target( analysis_summary_random_w_no_cens, 
                                           saveRDS( cbind( sim_scenarios_random,
                                                           tar_read(combined_random_no_cens)[tar_read(combined_random_no_cens)$pred == "W",]
                                           ),
                                           file = "./data/analysis_summary_random_no_cens_w.rds")
)

saved_summary_random_x_admin_cens <- tar_target( analysis_summary_random_x_admin_cens, 
                                              saveRDS( cbind( sim_scenarios_random,
                                                              tar_read(combined_random_admin_cens)[tar_read(combined_random_admin_cens)$pred == "X",]
                                              ),
                                              file = "./data/analysis_summary_random_admin_cens_x.rds")
)
saved_summary_random_w_admin_cens <- tar_target( analysis_summary_random_w_admin_cens, 
                                              saveRDS( cbind( sim_scenarios_random,
                                                              tar_read(combined_random_admin_cens)[tar_read(combined_random_admin_cens)$pred == "W",]
                                              ),
                                              file = "./data/analysis_summary_random_admin_cens_w.rds")
)
saved_summary_random_x_random_cens <- tar_target( analysis_summary_random_x_random_cens, 
                                               saveRDS( cbind( sim_scenarios_random,
                                                               tar_read(combined_random_random_cens)[tar_read(combined_random_random_cens)$pred == "X",]
                                               ),
                                               file = "./data/analysis_summary_random_random_cens_x.rds")
)

saved_summary_random_w_random_cens <- tar_target( analysis_summary_random_w_random_cens, 
                                               saveRDS( cbind( sim_scenarios_random,
                                                               tar_read(combined_random_random_cens)[tar_read(combined_random_random_cens)$pred == "W",]
                                               ),
                                               file = "./data/analysis_summary_random_random_cens_w.rds")
)
