# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Helper functions sensitivity analysis
# Prediction model validation, workhorse for sensitivity analysis and plotting
# -----------------------------------------------------------------------------#

# validation of prediction model ----

#' Estimation of measures of predictive performance to validate a prediction 
#' model for a time to event outcome (specifically, cox proportional hazard)
#' 
#' This function estimates the ratio of observed marginal risks at time point
#' t_val versus predicted marginal risks at time point t_val (cal_large) as a 
#' measure of calibration in the large, the time-dependent cumulative incidence 
#' area under the receiver operating characteristic curve (AUC(t)) at time point
#' t_val as a measure of discrimination, and the index of prediction accuracy 
#' (IPA(t)) at time poin t_val as a measure of overall accuracy.
#'
#' @param pred_names a character vector specifying the names of the predictors.
#' @param pred_coefs a numeric vector specifying the coefficients (not hazard
#' ratios) of the respective predictors as part of the model that is validated.
#' @param dataset the validation data set.
#' @param t_val the time point at which the model is validated.
#' @param baseline_surv the cumulative survival at t_val that was provided in 
#' the derivation study.
#'
#' @return the function outputs a single-row data.frame with three columns, 
#' containing the estimated O/E ratio (cal_large), AUC(t), and IPA(t).

validate_model <- function(pred_names,
                           pred_coefs,
                           dataset,
                           t_val,
                           baseline_surv){
  # compute centered linear predictor (prognostic index)
  lp <- as.matrix( dataset[ , pred_names]) %*% pred_coefs
  
  # calibration in the large
  
  # compute marginal predicted risk
  overall_surv <- baseline_surv ^ exp( lp)
  pred_risk <- 1 - overall_surv
  
  marginal_pred_risk <- mean( pred_risk)
  
  # observed marginal risk
  marginal_obs_risk <- 1 - summary( survfit( Surv( time, event) ~ 1, data = dataset), times= t_val)$surv
  
  cal_large <- marginal_pred_risk / marginal_obs_risk 
  
  # AUC(t_val)
  c_stat <- unname(
    timeROC::timeROC( T = dataset$time,
                      delta = dataset$event,
                      marker = lp,
                      cause = 1,
                      times = t_val)$AUC[2])
  # IPA (t_val)
  IPA <- riskRegression::IPA( pred_risk,
                              formula = Surv( time, event) ~ 1,
                              newdata = dataset,
                              times = t_val,
                              cause = 1)$IPA[2]
  
  results <- data.frame( "cal_large" = cal_large,
                         "AUC_t" = c_stat,
                         "IPA_t" = IPA)
  
  results <- cbind( results)
  
  return( results)
}

# workhorse sensitivity analysis ----

#' Helper function to perform a sensitivity analysis to assess the anticipated
#' impact of predictor measurement heterogeneity across validation and 
#' implementation setting.
#'
#' @param pmh_predictor a character string indicating for which predictor 
#' measurement heterogeneity is expected. The function works for a single 
#' predictor only (see manuscript for motivation).
#' @param psi a numeric vector specifying the range of anticipated additive 
#' systematic predictor measurement heterogeneity. Default value 0 indicates no 
#' additive systematic predictor measurement heterogeneity.
#' @param theta a numeric vector specifying the range of anticipated 
#' multiplicative systematic predictor measurement heterogeneity. Default value 
#' 1 indicates no multiplicative systematic predictor measurement heterogeneity.
#' @param sd_epsilon a numeric vector specifying the range of anticipated random 
#' predictor measurement heterogeneity. Default value 0 indicates no random
#' predictor measurement heterogeneity.
#' @param validation_data the validation data set.
#' @param pred_names a character vector specifying the names of the predictors.
#' @param pred_coefs a numeric vector specifying the coefficients (not hazard
#' ratios) of the respective predictors as part of the model that is validated.
#' @param t_val the time point at which the model is validated.
#' @param baseline_surv the cumulative survival at t_val that was provided in 
#' the derivation study.
#' @param bootstrap_rep the total number of bootstrap repetitions.
#'
#' @return the function outputs a named numeric vector of length 9 containing
#' the estimated O/E ratio (cal_large), AUC(t), and IPA(t) with bootstrapped 
#' 95 percentile confidence intervals.

pmh_sensitivity_analysis <- function( pmh_predictor,
                                      psi = 0,
                                      theta = 1,
                                      sd_epsilon = 0,
                                      validation_data,
                                      pred_names,
                                      pred_coefs,
                                      t_val,
                                      baseline_surv,
                                      bootstrap_rep = 500 
                                      ){
  # check variable names
  if( ! "time" %in% colnames( validation_data) ) { stop( "Rename time variable as 'time'.") }
  if( ! "event" %in% colnames( validation_data) ) { stop( "Rename event variable as 'event'.") }

  # generate predictor measurement heterogeneity
  pmh_data <- validation_data
  epsilon <- rnorm( n = nrow( validation_data), mean = 0, sd = sd_epsilon)
  pmh_data[, pmh_predictor] <- psi + theta * validation_data[, pmh_predictor] + epsilon
  
  point_estimates <- validate_model( dataset = pmh_data,
                                     pred_names = pred_names,
                                     pred_coefs = pred_coefs,
                                     t_val = t_val,
                                     baseline_surv = baseline_surv)
  # bootstrap confidence intervals
  bootstrapped_estimates <- matrix( NA, 
                                    nrow = bootstrap_rep, 
                                    ncol = ncol( point_estimates))
  
  for(i in 1:bootstrap_rep){
    bootstrap_data <- pmh_data[ sample( 1:nrow( pmh_data),
                                               size = nrow( pmh_data),
                                               replace = T), ]
    
    bootstrapped_estimates[i, ] <- unlist( 
      validate_model( dataset = bootstrap_data,
                      pred_names = pred_names,
                      pred_coefs = pred_coefs,
                      t_val = t_val,
                      baseline_surv = baseline_surv))
  }
  
  lower_bounds <- apply( bootstrapped_estimates,
                         2,
                         function( x) quantile( x, 0.025) )
  upper_bounds <- apply( bootstrapped_estimates,
                         2,
                         function( x) quantile( x, 0.975) )
  
  results <- cbind( point_estimates,
                    t( lower_bounds),
                    t( upper_bounds) )
  colnames( results) <- c( colnames( point_estimates),
                           paste0( "ci_low_", colnames( point_estimates)),
                           paste0( "ci_up_", colnames( point_estimates)))
  
  return( results)
  
}


# plotting sensitivity analysis ----

prepare_plotting_data <- function( input_data,
                                   parameter_name){
  
  # group by random pmh
  temp1 <- reshape( input_data[ , c( "psi", "theta", "sd_epsilon",
                                     parameter_name)],
                    timevar = "sd_epsilon",
                    idvar = c( "psi", "theta"),
                    direction = "wide")
  processed_dat <- data.frame(
    sd_epsilon = unique( scenarios$sd_epsilon),
    y.lower = apply( temp1[, !( colnames( temp1) %in% c( "psi", "theta"))],
                     2, min),
    y.upper = apply( temp1[, !( colnames( temp1) %in% c( "psi", "theta"))],
                     2, max)
  )
  
  temp2 <- reshape( input_data[, c("psi", "theta", "sd_epsilon",
                                   paste0("ci_low_", parameter_name))],
                    timevar = "sd_epsilon",
                    idvar = c("psi", "theta"),
                    direction = "wide")
  processed_dat$low_ci <- 
    apply( temp2[, !( colnames( temp2) %in% c( "psi", "theta"))],
           2, min)
  
  temp3 <- reshape( input_data[, c("psi", "theta", "sd_epsilon",
                                   paste0("ci_up_", parameter_name))],
                    timevar = "sd_epsilon",
                    idvar = c("psi", "theta"),
                    direction = "wide")
  processed_dat$up_ci <- 
    apply( temp3[, !( colnames( temp3) %in% c( "psi", "theta"))],
           2, max)
  
  return( processed_dat)
}

plotting_data <- function( plot_data){
  # prepare calibration in the large data
  cal_large_dat <- prepare_plotting_data( input_data = plot_data,
                                          parameter_name = "cal_large")
  
  # plot calibration in the large
  p1 <- ggplot( cal_large_dat, aes( x = sd_epsilon)) +
    geom_ribbon( aes( ymax=y.upper, ymin=y.lower), fill="black", alpha=.5) +
    geom_ribbon( aes( ymax=up_ci, ymin=low_ci), fill="black", alpha=.1) +
    theme_classic() + 
    geom_hline( aes(yintercept= homogeneity_coordinate$cal_large,
                    linetype = "Predictor measurement \nhomogeneity"), colour= "black") +
    scale_linetype_manual(name = " ", values = 2, 
                          guide = guide_legend( override.aes = list(color = c("black")))) +
    ylab( "O / E ratio at 6 years") +
    xlab( " ") +
    ylim( 0.3, 1.4) +
    labs( title = "Calibration in the large") +
    theme( legend.position = c(1.25, 1),
           axis.text=element_text(size=14, family="serif"),
           axis.title=element_text(size=20, family="serif"),
           legend.text=element_text(size=20, family="serif"),
           plot.title=element_text(size=26, family="serif", face = "bold"),
           axis.title.y = element_text(angle=0),
           axis.line = element_line(color = "#E5E5E5"),
           axis.ticks = element_line(color = "#E5E5E5"),
           plot.margin = unit(c(55, 220, 5.5, 5), "points")) 
  
  
  # prepare AUC(t) data
  c_stat_dat <- prepare_plotting_data( input_data = plot_data,
                                       parameter_name = "AUC_t")
  # plot AUC(t)
  p2 <- ggplot( c_stat_dat, aes( x = sd_epsilon)) +
    geom_ribbon( aes( ymax=y.upper, ymin=y.lower), fill="black", alpha=.5) +
    geom_ribbon( aes( ymax=up_ci, ymin=low_ci), fill="black", alpha=.1) +
    theme_classic() + 
    geom_hline( yintercept = homogeneity_coordinate$AUC_t,
                linetype = 2) +
    ylab( "AUC(6 years)") +
    xlab( "") +
    ylim( 0.8, 1)+
    labs( title = "Discrimination") +
    theme( legend.position = "none",
           axis.text=element_text(size=14, family="serif"),
           axis.title=element_text(size=20, family="serif"),
           plot.title=element_text(size=26, family="serif", face = "bold"),
           axis.title.y = element_text(angle=0),
           axis.line = element_line(color = "#E5E5E5"),
           axis.ticks = element_line(color = "#E5E5E5"),
           plot.margin = unit(c(55, 220, 5.5, 55), "points")) 
  
  # prepare IPA(t) data
  accuracy_dat <- prepare_plotting_data( input_data = plot_data,
                                         parameter_name = "IPA_t")
  
  # plot IPA(t)
  p3 <- ggplot( accuracy_dat, aes( x = sd_epsilon)) +
    geom_ribbon( aes( ymax=y.upper, ymin=y.lower), fill="black", alpha=.5) +
    geom_ribbon( aes( ymax=up_ci, ymin=low_ci), fill="black", alpha=.1) +
    theme_classic() + 
    geom_hline( yintercept = homogeneity_coordinate$IPA_t,
                linetype = 2) +
    ylab( "IPA(6 years)") +
    xlab( "Random measurement heterogeneity (sd of error term)") +
    ylim( -0.05,0.2) +
    labs( title = "Index of prediction accuracy") +
    theme( legend.position = "none",
           axis.text=element_text(size=14, family="serif"),
           axis.title=element_text(size=20, family="serif"),
           plot.title=element_text(size=26, family="serif", face = "bold"),
           axis.title.y = element_text(angle=0),
           axis.line = element_line(color = "#E5E5E5"),
           axis.ticks = element_line(color = "#E5E5E5"),
           axis.title.x = element_text(margin = margin(t = 20)),
           plot.margin = unit(c(55, 220, 5.5, 55), "points")) 
  
  # combine plots
  total_plot <- cowplot::plot_grid( plotlist = list( p1, p2, p3),
                                    nrow = 3)
  
  pdf("./plot/sensitivity_analysis_plot.pdf", width = 12, height = 20)
  print( total_plot)
  dev.off()
  
}
