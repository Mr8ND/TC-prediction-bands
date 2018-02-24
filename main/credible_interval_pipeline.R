#' Library ----------------------------------------

#library(datamart)
#library(geosphere)
#library(plyr)
#library(rworldmap)
#library(caret)
#library(ks)
#library(gtools)
#library(RANN)


#' Sourcing external functions ----------------------------------------

#functions_loc <- '../TCcrediblebands/R/'
#source(paste0(functions_loc, 'kde_functions.R'))
#source(paste0(functions_loc, 'bubble_functions.R'))
#source(paste0(functions_loc, 'convex_hull.R'))
#source(paste0(functions_loc, 'delta_ball.R'))
#source(paste0(functions_loc, 'thirteen_point_compression.R'))
#source(paste0(functions_loc, 'path_functions.R'))
#source(paste0(functions_loc, 'depth_function.R'))
#source(paste0(functions_loc, 'credible_interval_pipeline_functions.R'))

# Install from Github ----------------------------------

library(devtools)
devtools::install_github(repo = 'Mr8ND/Hurricanes_701/TCcrediblebands')
library(TCcrediblebands)

#' Execution --------------------------------------------------------

load('data/sample/Test_Sims_1000.Rdata')

#complexity_mat_results <- matrix(rep(NA,16), ncol=2, nrow=8)
#num_samples_vec <- c(50, 100, 150, 250, 350, 500, 750, 1000)


#for (idx_sim in c(1:8)) {
reduced_test_env <- list("AL031951" = list("Auto_DeathRegs"= list(),
                                           "Auto_NoDeathRegs" = list(),
                                           "NoAuto_DeathRegs" = list(),
                                           "NoAuto_NoDeathRegs" = list()),
                         "AL011951" = list("Auto_DeathRegs" = list(),
                                           "Auto_NoDeathRegs" = list(),
                                           "NoAuto_DeathRegs" = list(),
                                           "NoAuto_NoDeathRegs" = list()))

for (name_tc in names(reduced_test_env)) {
  for (curve_type in names(reduced_test_env[[name_tc]])) {
    for (i in c(1:10)) { # or num_samples_vec[idx_sim] if we are testing for complexity
      reduced_test_env[[name_tc]][[curve_type]][[i]] <- test_env[[name_tc]][[curve_type]][[i]]
    }
  }
}


# Data import step as dflist - list of lists of TCs
tc_full_sim_list <- reduced_test_env

# Data import step for true TCs DF - test ones
tc_true_path_list <- list("AL031951" = test_env$AL031951$Auto_DeathRegs[[1]],
                          "AL011951" = test_env$AL011951$Auto_DeathRegs[[1]])
alpha_level <- 0.1

start.time <- Sys.time()
output_pipeline <- credible_interval_pipeline(tc_full_sim_list = tc_full_sim_list,
                                                tc_true_path_list = tc_true_path_list,
                                                alpha_level = alpha_level)
end.time <- Sys.time()
time.taken <- end.time - start.time

print(c(idx_sim, as.numeric(time.taken)))

complexity_mat_results[idx_sim,] <- c(num_samples_vec[idx_sim], as.numeric(time.taken)/2)
#}

#save(complexity_mat_results, file = 'data/complexit_mat_results.Rdata')


out_filename <- paste0('data/output_pipeline_alpha', as.character(alpha_level), '_', Sys.Date(), '.Rdata')
save(output_pipeline, file = out_filename)

