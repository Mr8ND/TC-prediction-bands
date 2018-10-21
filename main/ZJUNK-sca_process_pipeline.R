# Taking in commands for the command line (use Rscript file a b)
# what do the parameters mean:
# a = integer, index of start of curves to run for
# b = integer, index of final curve number to run for

args <- commandArgs(trailingOnly = TRUE)
start <- round(as.numeric(args[1]))
finish <- round(as.numeric(args[2]))

# Locations  -------------------------
data_loc  <- "main/data/"

# Install from Github ----------------------------------

library(devtools)
library(methods)
#devtools::install_github(repo = 'benjaminleroy/Hurricanes_701/TCpredictionbands')
#devtools::install_github(repo = 'Mr8ND/Hurricanes_701/TCpredictionbands')
library(TCpredictionbands)


# loading SCA Structure ------------------

load(file = paste0(data_loc,"sca_training_structure.Rdata")) 
load(file = paste0(data_loc,"sca_model_structure.Rdata")) 

# Loading and formating simulated curves ----------------------

load(paste0(data_loc,'/Test_Sims_350.Rdata'))

desired_curves_types <- c("Auto_DeathRegs","Auto_NoDeathRegs",
                          "NoAuto_DeathRegs", "NoAuto_NoDeathRegs")


test_tc_names <- names(test_env)[start:finish]

n_curves = 350 #number of simulated curves 

reduced_test_env <- list()
for (name_tc in test_tc_names) {
  reduced_test_env[[name_tc]] <- list()
  for (curve_type in desired_curves_types) {
    for (i in c(1:n_curves)) { 
      reduced_test_env[[name_tc]][[curve_type]][[i]] <- 
                              test_env[[name_tc]][[curve_type]][[i]]
    }
  }
}

# Applying the sca_projection to simulated curves -----------------

test_data_list <- reduced_test_env

projection_out <- sca_projection(test_data_list,
                                 train_list_13_point,
                                 train_info,
                                 position = 1:2,
                                 verbose = TRUE)

save(projection_out, file = paste0(data_loc, "sca_projection_info",
                                   as.character(start), "_",
                                   as.character(finish), ".Rdata"))

