# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Create plots
# -----------------------------------------------------------------------------#
input <- readRDS("./data/analysis_summary_add_no_cens_w.rds")

# Helper functions

group_data <- function( data_input){
  # select variables to define grouping levels
  var1 <- if( length( unique( data_input$psi)) == 3) { print( "psi")}
  var2 <- if( length( unique( data_input$theta)) == 3) { print( "theta")}
  var3 <- if( length( unique( data_input$sigma_epsilon)) == 3) { print( "sigma_epsilon")}
  
  relevant_vars <- c( var1, var2, var3)
  
  # define grouping levels
  data_input$grouping_level <- NA
  data_input$grouping_level[ data_input[ , relevant_vars[1] ] == max( data_input[ , relevant_vars[1] ]) & 
                          data_input[ , relevant_vars[2] ] == max( data_input[ , relevant_vars[2] ])] <- 1
  data_input$grouping_level[ data_input[ , relevant_vars[1] ] == max( data_input[ , relevant_vars[1] ]) & 
                          data_input[ , relevant_vars[2] ] == median( data_input[ , relevant_vars[2] ])] <- 2
  data_input$grouping_level[ data_input[ , relevant_vars[1] ] == median( data_input[ , relevant_vars[1] ]) & 
                          data_input[ , relevant_vars[2] ] == max( data_input[ , relevant_vars[2] ])] <- 3
  data_input$grouping_level[ data_input[ , relevant_vars[1] ] == median( data_input[ , relevant_vars[1] ]) & 
                          data_input[ , relevant_vars[2] ] == median( data_input[ , relevant_vars[2] ])] <- 4
  data_input$grouping_level[ data_input[ , relevant_vars[1] ] == min( data_input[ , relevant_vars[1] ]) & 
                          data_input[ , relevant_vars[2] ] == median( data_input[ , relevant_vars[2] ])] <- 5
  data_input$grouping_level[ data_input[ , relevant_vars[1] ] == median( data_input[ , relevant_vars[1] ]) & 
                          data_input[ , relevant_vars[2] ] == min( data_input[ , relevant_vars[2] ])] <- 6
  data_input$grouping_level[ data_input[ , relevant_vars[1] ] == min( data_input[ , relevant_vars[1] ]) & 
                          data_input[ , relevant_vars[2] ] == min( data_input[ , relevant_vars[2] ])] <- 7
  
  return(data_input)
}

test<- group_data(data_input = input)

ggplot(test,
       aes( x = ( test[, "theta"] / 10), y = cal_large, group = grouping_level)) +
  geom_line() + 
  theme_classic() + 
  ylab( "Calibration-in-the-large coefficient") +
  xlab( parse(text = paste0("theta"))) +
  ylim( c(-0.5, 0.5)) +
  geom_hline( yintercept = 0, linetype = "dotted")


ggplot(group_data(data_input = input),
       aes( x = ( ( psi / 10) - 0.3), y = cal_slope, group = grouping_level)) +
  geom_line() + 
  theme_classic() + 
  ylab( "Calibration slope") +
  xlab( expression(psi)) +
  ylim( c(0, 2)) +
  geom_hline( yintercept = 1, linetype = "dotted")


ggplot(group_data(data_input = input),
       aes( x = ( ( psi / 10) - 0.3), y = c_stat, group = grouping_level)) +
  geom_line() + 
  theme_classic() + 
  ylab( "C-statistic") +
  xlab( expression(psi)) +
  ylim( c(0.6, 0.9)) +
  geom_hline( yintercept = 0.705, linetype = "dotted")

ggplot(group_data(data_input = input),
       aes( x = ( ( psi / 10) - 0.3), y = IPA, group = grouping_level)) +
  geom_line() + 
  theme_classic() + 
  ylab( "Index of Prediction Accuracy") +
  xlab( expression(psi)) +
  ylim( c(0, 0.5)) +
  geom_hline( yintercept = 0.13, linetype = "dotted")







plot_function <- function( plot_data,
                           x_axis_parameter,
                           x_axis_label){
  p1 <- ggplot(plot_data,
         aes( x = x_axis_parameter, y = cal_large, group = grouping_level)) +
    geom_line() + 
    theme_classic() + 
    ylab( "Calibration-in-the-large coefficient") +
    xlab( parse(text = paste0(x_axis_label))) +
    ylim( c(-0.5, 0.5)) 


  p2 <- ggplot(plot_data,
         aes( x = x_axis_parameter, y = cal_slope, group = grouping_level)) +
    geom_line() +
    theme_classic() +
    ylab( "Calibration slope") +
    xlab( parse(text = paste0(x_axis_label))) +
    ylim( c(0, 2))


  p3 <- ggplot(plot_data,
         aes( x = x_axis_parameter, y = c_stat, group = grouping_level)) +
    geom_line() +
    theme_classic() +
    ylab( "C-statistic") +
    xlab( parse(text = paste0(x_axis_label))) +
    ylim( c(0.6, 0.9))

  p4 <- ggplot(plot_data,
         aes( x = x_axis_parameter, y = IPA, group = grouping_level)) +
    geom_line() +
    theme_classic() +
    ylab( "Index of Prediction Accuracy") +
    xlab( parse(text = paste0(x_axis_label))) +
    ylim( c(0, 0.5))

  cowplot::plot_grid(plotlist = list(p1, p2, p3, p4), nrow = 1)
}

# Figure for main text
# ### this would make sense if I didn't have to do the weird parametrizaton for targets package
# make_figure_helper <- function(file,
#                                x_axis_parameter){
#    input <- readRDS(file)
#    
#    plot_data <- group_data(data_input = input)
#    
#    plot_row <- plot_function( plot_data = plot_data,
#                               x_axis_parameter = x_axis_parameter,
#                               x_axis_label = x_axis_label)
#  }


# make_figure_maintext <- function(){
#   p1 <- make_figure_helper("./data/analysis_summary_add_no_cens_w.rds",
#                            x_axis_parameter = "psi")
#   p2 <- make_figure_helper("./data/analysis_summary_mult_no_cens_w.rds",
#                            x_axis_parameter = "theta")
#   p3 <- make_figure_helper("./data/analysis_summary_random_no_cens_w.rds",
#                            x_axis_parameter = "sigma_epsilon")
# 
#   total_plot <- grid.arrange(p1, p2, p3,
#                              nrow = 3)
# 
#   pdf("./plot/maintext_figure.pdf")
#   total_plot
#   dev.off()
# }

input     <- readRDS("./data/analysis_summary_add_no_cens_w.rds")
plot_data <- group_data(data_input = input)
row_1     <- plot_function( plot_data = plot_data,
                            x_axis_parameter = (plot_data$psi / 10) - 0.3,
                            x_axis_label = "psi")
input     <- readRDS("./data/analysis_summary_mult_no_cens_w.rds")
plot_data <- group_data(data_input = input)
row_2     <- plot_function( plot_data = plot_data,
                            x_axis_parameter = (plot_data$theta / 10),
                            x_axis_label = "theta")
input     <- readRDS("./data/analysis_summary_random_no_cens_w.rds")
plot_data <- group_data(data_input = input)
row_3     <- plot_function( plot_data = plot_data,
                            x_axis_parameter = (plot_data$sigma_epsilon / 10),
                            x_axis_label = "sigma[epsilon]")
total_plot <- cowplot::plot_grid(plotlist = list(row_1, row_2, row_3), nrow = 3)

pdf("./plot/maintext_figure.pdf", width = 20, height = 12)
total_plot
dev.off()

 
# "./data/analysis_summary_add_admin_cens_w.rds"
# "./data/analysis_summary_add_random_cens_w.rds" See that censoring affects calibration even under no pmh

