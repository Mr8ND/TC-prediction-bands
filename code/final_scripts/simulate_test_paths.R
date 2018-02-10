library(geosphere)
library(plyr)
library(lubridate)

functions_loc = "code/functions/"
source(paste0(functions_loc, "Regression_functions.R")) 
source(paste0(functions_loc, "creating_curves.R"))
source(paste0(functions_loc, "read_data_source.R"))

ptm <- proc.time()

test_sims <- generate_all()

proc.time() - ptm

# Save environment w/ one list per test TC.
# Each list has length 4 (one for each auto/non-auto death_reg/no_death_reg combo)
# Each of those 4 lists have length 1000 (all simulations from given starting observations)
test_env <- list2env(test_sims)
save(test_env, file = "data/generate/Test_Sims_1000.Rdata")
