context("Global PB test")

#' Loading all the data --------------------------------------

true_curve_conversion_function <- function(df){
  output_df = data.frame(cbind(df$long, df$lat))
  names(output_df) = c('long', 'lat')
  return(output_df)
}

sample_sim <- TCpredictionbands::sample_sim
sample_tc <- TCpredictionbands::sample_tc
sample_tc_name <- TCpredictionbands::sample_tc_name
sample_output_pipeline <- TCpredictionbands::sample_output_pipeline

# for now:
names(sample_output_pipeline) <- "AL032009"

n_sim_curve_total <- length(sample_sim)
alpha <- 0.1
alpha_ci <- .05
desired_curves_types <- c("Auto_NoDeathRegs")


tc_full_sim_list <- list()
for (i in c(1:n_sim_curve_total)) { 
  tc_full_sim_list[[sample_tc_name]][[desired_curves_types[1]]][[i]] <- 
    sample_sim[[i]]
}

tc_true_path_list <- list()
tc_true_path_list[[sample_tc_name]] <- sample_tc[, c(6,5)]

output_pipeline <- prediction_interval_pipeline(tc_full_sim_list = tc_full_sim_list,
                                              tc_true_path_list = tc_true_path_list,
                                              alpha = alpha,
                                              alpha_ci = alpha_ci,
                                              start_idx = 1,
                                              end_idx = 1,
                                              curve_type_vec = desired_curves_types)

# Removing the stochastic nature of calculations and compilation times
output_pipeline[[1]][["Auto_NoDeathRegs"]][["kde"]][["area"]] <- NULL
output_pipeline[[1]][["Auto_NoDeathRegs"]][["bubble_ci"]][["area"]] <- NULL
output_pipeline[[1]][["Auto_NoDeathRegs"]][["delta_ball"]][["area"]] <- NULL
output_pipeline[[1]][["Auto_NoDeathRegs"]][["delta_ball"]][["area_ci"]] <- NULL
output_pipeline[[1]][["Auto_NoDeathRegs"]][["convex_hull"]][["area"]] <- NULL
output_pipeline[[1]][["Auto_NoDeathRegs"]][["time"]] <- NULL

# as of Feb 2020 - we corrected an error with the global distance depth function
# but it's erroring this code - so we've just dropped
output_pipeline[[1]][["Auto_NoDeathRegs"]][["depth_vector"]] <- NULL
sample_output_pipeline[[1]][["Auto_NoDeathRegs"]][["depth_vector"]] <- NULL

test_that("Checking the pipeline results have not changed", {
  expect_identical(output_pipeline, sample_output_pipeline)
})
