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

latest_full_output_pipeline <- 'output_pipeline_alphalevel0.1_complete_2018-03-20'

data_loc <- "main/data/"
load(paste0(data_loc, latest_full_output_pipeline))
#load(paste0(data_loc, 'Validation_sim.Rdata'))
#let validation_sim be the list of actual simulations

#' Execution --------------------------------------------------------

desired_curves_types <- c("Auto_DeathRegs","Auto_NoDeathRegs",
                          "NoAuto_DeathRegs", "NoAuto_NoDeathRegs")

simulation_validation_pipeline <- list()
for (name_tc in names(output_list_pipeline)){
	for (curve_type in desired_curves_types){
		simulation_validation_pipeline[[name_tc]][[curve_type]] <- calculate_invec_per_method(
						hur_out_obj = output_list_pipeline[[name_tc]][[curve_type]],
					    sim_hur_list = lapply(validation_sim[[name_tc]][[curve_type]], data.frame)
					    )
	}
}

out_filename <- paste0('main/data/',
                       'sim_validation_results_',
                       Sys.Date(), '.Rdata')
save(simulation_validation_pipeline, file = out_filename)