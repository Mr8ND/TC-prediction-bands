# create 2 additional sets of simulated curves for PB analysis
library(methods)
library(TCpredictionbands)
load("main/data/raw_data.Rdata") # contains train_data and test_data

# Generate 100 additional simulated curves for each test TC -------------

set.seed(100)

test_sims <- generate_all(train = train_data, 
                          test = test_data, 
                          number_paths = 100, 
                          replicate = F)

test_env <- list2env(test_sims)
save(test_env, file = "main/data/Test_Sims_100_addl.Rdata")

# Generate 75 additional simulated curves for each test TC -------------

set.seed(648)

test_sims <- generate_all(train = train_data, 
                          test = test_data,
                          number_paths = 75, 
                          replicate = F)

test_env <- list2env(test_sims)
save(test_env, file = "main/data/Test_Sims_75_addl.Rdata")
