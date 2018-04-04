library(methods)
library(TCcrediblebands)
#functions_loc = "../TCcrediblebands/R/"
#source(paste0(functions_loc, "regression_functions.R")) 
#source(paste0(functions_loc, "creating_curves.R"))
#source(paste0(functions_loc, "read_data_source.R"))
#load("../data/raw_data.Rdata") # contains train_data and test_data

ptm <- proc.time()

set.seed(100)

test_sims <- generate_all(train = train_data, test = test_data, remove_length_2 = T, 
                          number_paths = 100, replicate = F, verbose = T)

proc.time() - ptm

# Save environment w/ one list per test TC.
# Each list has length 4 (one for each auto/non-auto death_reg/no_death_reg combo)
# Each of those 4 lists have length 100 (all simulations from given starting observations)
test_env <- list2env(test_sims)
save(test_env, file = "../data/Test_Sims_100_addl.Rdata")
