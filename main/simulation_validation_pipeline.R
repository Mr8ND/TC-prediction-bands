# sim_file_name: the simulation rdata file that stores all the curves
# example:
# Rscript main/simulation_validation_pipeline.R Test_Sims_100_addl.rdata

#' Library ----------------------------------------
library(methods)
library(tidyverse)

#' Install from Github ----------------------------------

library(TCcrediblebands)

#' Getting information from user ---------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  sim_file_name <- args[1] #  'Test_Sims_100_addl.rdata'
}


#' Loading all the data --------------------------------------

true_curve_conversion_function <- function(df){
  output_df = data.frame(cbind(df$long, df$lat))
  names(output_df) = c('long', 'lat')
  return(output_df)
}

latest_full_output_pipeline <- 'output_pipeline_alphalevel0.1_all.Rdata'

data_loc <- "main/data/"
a <- load(paste0(data_loc, latest_full_output_pipeline)) #output_list_pipeline
eval(parse(text = paste0("output_list_pipeline <- ",a)))

load(paste0(data_loc, sim_file_name)) #test_env
amount <- test_env[[names(test_env)[1]]][[1]] %>% length


if (is.null(names(output_list_pipeline))) { 
  warning(paste("output_list_pipeline list missing names.",
          "Renaming output_list_pipeline's entries with test env's names"))
  names(output_list_pipeline) = names(test_env)
} else {
  if (!(all(names(test_env) == names(output_list_pipeline)))) {
    stop(paste(
       "Names of the test_env and output_list_pipeline objects need to be the",
       "same"))
  }
}

#' Execution --------------------------------------------------------

pb <- progress::progress_bar$new(
  format = "Simulation Validation Pipeline [:bar] :percent eta: :eta",
  total = length(test_env), clear = FALSE, width = 51)

desired_curves_types <- c("Auto_DeathRegs","Auto_NoDeathRegs",
                          "NoAuto_DeathRegs", "NoAuto_NoDeathRegs")

simulation_validation_pipeline <- list()

for (name_tc in names(output_list_pipeline)) {
	for (curve_type in desired_curves_types) {
		simulation_validation_pipeline[[name_tc]][[curve_type]] <- calculate_invec_per_method(
						hur_out_obj = output_list_pipeline[[name_tc]][[curve_type]],
					  sim_hur_list = lapply(test_env[[name_tc]][[curve_type]], data.frame)
					    )
    }
  pb$tick()
}

out_filename <- paste0('main/data/',
                       'sim_validation_results',amount,'_',
                       Sys.Date(), '.Rdata')
save(simulation_validation_pipeline, file = out_filename)
