library(TCpredictionbands)

# loading processed data -------------
load("main/data/raw_data.Rdata")

ptm <- proc.time()
test_sims <- generate_all(train = train_data,
                          test = test_data)
gen_time_350 = proc.time() - ptm

# Save environment w/ one list per test TC.
# Each list has length 4 
#   (one for each auto/non-auto death_reg/no_death_reg combo)
# Each of those 4 lists have length 350 
#   (all simulations from given starting observations)
test_env <- list2env(test_sims)
save(test_env, gen_time_350, file = "main/data/Test_Sims_350.Rdata")
