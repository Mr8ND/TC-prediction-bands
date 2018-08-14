#' Library ----------------------------------------
library(devtools)
library(methods)

#' Install from Github ----------------------------------

#devtools::install_github(repo = 'Mr8ND/Hurricanes_701/TCcrediblebands')
library(TCcrediblebands)

#' Loading all the data --------------------------------------

true_curve_conversion_function <- function(df){
  output_df = data.frame(cbind(df$long, df$lat))
  names(output_df) = c('long', 'lat')
  return(output_df)
}

data_loc <- "main/data/"
load(paste0(data_loc, 'Test_Sims_350.Rdata'))
load(paste0(data_loc, 'raw_data.Rdata'))

true_curve_test <- lapply(test_data, true_curve_conversion_function)


#' Getting the initial arguments -----------------------------

start_idx_passed <- 1
end_idx_passed <- length(test_env)
n_sim_curve_total <- 350
alpha_level <- 0.1
alpha_ci_level <- .05

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  start_idx_passed <- args[1]
  end_idx_passed <- args[2]
}

#' Execution --------------------------------------------------------

desired_curves_types <- c("Auto_DeathRegs","Auto_NoDeathRegs",
                          "NoAuto_DeathRegs", "NoAuto_NoDeathRegs")

tc_full_sim_list <- list()
tc_true_path_list <- list()
for (name_tc in names(test_env)) {
  for (curve_type in desired_curves_types) {
    for (i in c(1:n_sim_curve_total)) { 
      tc_full_sim_list[[name_tc]][[curve_type]][[i]] <- 
                                          test_env[[name_tc]][[curve_type]][[i]]
      tc_true_path_list[[name_tc]] <- true_curve_test[[name_tc]]
    }
  }
}


output_pipeline <- credible_interval_pipeline(tc_full_sim_list = tc_full_sim_list,
                                          tc_true_path_list = tc_true_path_list,
                                          alpha_level = alpha_level,
                                          alpha_ci_level = alpha_ci_level,
                                          start_idx = start_idx_passed,
                                          end_idx = end_idx_passed,
                                          curve_type_vec = desired_curves_types)

out_filename <- paste0(data_loc,
                       'output_pipeline_alphalevel',
                       as.character(alpha_level), '_',
                       start_idx_passed, '_',
                       end_idx_passed, '_',
                       Sys.Date(), '.Rdata')
save(output_pipeline, file = out_filename)