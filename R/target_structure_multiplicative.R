# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Helper file structure targets multiplicative predictor measurement heterogeneity (pmh)
# -----------------------------------------------------------------------------#

# Define simulation scenarios of multiplicative pmh structures ----

generate_sys_mult_scenarios <- function(psi           = c( 0, 0.3, 0.6),
                                        theta         = c( 0.5, 0.7, 0.8, 1, 1.3, 1.7, 2),
                                        sigma_epsilon = c( 0, 1, 2)
                                        ){
  # create scenario grid
  sys_mult_scenarios <- expand.grid( psi          = psi,
                                     theta         = theta,
                                     sigma_epsilon = sigma_epsilon)
  
  # remove redundant scenarios
  sys_mult_scenarios <- subset( sys_mult_scenarios, (!( psi == max( psi) & 
                                                          sigma_epsilon == min( sigma_epsilon)) &
                                                       !( psi == min( psi) &
                                                            sigma_epsilon == max( sigma_epsilon))))
  # correction for targets naming
  sys_mult_scenarios <- sys_mult_scenarios * 10
  
  return(sys_mult_scenarios)
}

sim_scenarios_sys_mult <- generate_sys_mult_scenarios()


# Define static branches for targets ----

multiplicative <- tar_map(
  unlist = FALSE, # Return a nested list from tar_map() for combining later
  values = tibble( generate_sys_mult_scenarios()), # map over simulation scenarios
  
  # simulate data without censoring
  tar_target( mult_data_no_cens, 
              generate_data_no_cens( psi = psi,
                                     theta = theta,
                                     sigma_epsilon = sigma_epsilon)),
  
  # analyze uncensored data
  tar_target( mult_results_no_cens, 
              analyse_data( data = mult_data_no_cens),
              format = "rds"),
  
  # introduce administrative censoring at t = 15
  tar_target( mult_data_admin_cens, 
              generate_data_admin_cens( data_no_censoring = mult_data_no_cens,
                                        c_admin = 15)),
  
  # analyze administratively censored data
  tar_target( mult_results_admin_cens, 
              analyse_data( data = mult_data_admin_cens),
              format = "rds"),
  
  # introduce random censoring
  tar_target( mult_data_random_cens, 
              generate_data_random_cens( data_admin_censoring = mult_data_admin_cens,
                                         data_no_censoring = mult_data_no_cens)),
  # analyze randomly censored data
  tar_target( mult_results_random_cens, 
              analyse_data( data = mult_data_random_cens),
              format = "rds")
)

# Define combined targets (rbind all results) ----

combined_mult_nocens <- tar_combine(
  combined_mult_no_cens,
  multiplicative[[2]],
  command = dplyr::bind_rows(!!!.x)
)

combined_mult_censadmin <- tar_combine(
  combined_mult_admin_cens,
  multiplicative[[4]],
  command = dplyr::bind_rows(!!!.x)
)

combined_mult_censrandom <- tar_combine(
  combined_mult_random_cens,
  multiplicative[[6]],
  command = dplyr::bind_rows(!!!.x)
)

# Save output ----

saved_summary_mult_x_no_cens <- tar_target( analysis_summary_mult_x_no_cens, 
                                       saveRDS( cbind( sim_scenarios_sys_mult,
                                                       tar_read(combined_mult_no_cens)[tar_read(combined_mult_no_cens)$pred == "X",]
                                       ),
                                       file = "./data/analysis_summary_mult_no_cens_x.rds")
)
saved_summary_mult_w_no_cens <- tar_target( analysis_summary_mult_w_no_cens, 
                                       saveRDS( cbind( sim_scenarios_sys_mult,
                                                       tar_read(combined_mult_no_cens)[tar_read(combined_mult_no_cens)$pred == "W",]
                                       ),
                                       file = "./data/analysis_summary_mult_no_cens_w.rds")
)

saved_summary_mult_x_admin_cens <- tar_target( analysis_summary_mult_x_admin_cens, 
                                          saveRDS( cbind( sim_scenarios_sys_mult,
                                                          tar_read(combined_mult_admin_cens)[tar_read(combined_mult_admin_cens)$pred == "X",]
                                          ),
                                          file = "./data/analysis_summary_mult_admin_cens_x.rds")
)
saved_summary_mult_w_admin_cens <- tar_target( analysis_summary_mult_w_admin_cens, 
                                          saveRDS( cbind( sim_scenarios_sys_mult,
                                                          tar_read(combined_mult_admin_cens)[tar_read(combined_mult_admin_cens)$pred == "W",]
                                          ),
                                          file = "./data/analysis_summary_mult_admin_cens_w.rds")
)
saved_summary_mult_x_random_cens <- tar_target( analysis_summary_mult_x_random_cens, 
                                           saveRDS( cbind( sim_scenarios_sys_mult,
                                                           tar_read(combined_mult_random_cens)[tar_read(combined_mult_random_cens)$pred == "X",]
                                           ),
                                           file = "./data/analysis_summary_mult_random_cens_x.rds")
)

saved_summary_mult_w_random_cens <- tar_target( analysis_summary_mult_w_random_cens, 
                                           saveRDS( cbind( sim_scenarios_sys_mult,
                                                           tar_read(combined_mult_random_cens)[tar_read(combined_mult_random_cens)$pred == "W",]
                                           ),
                                           file = "./data/analysis_summary_mult_random_cens_w.rds")
)
