# Code to save .Rdata file with lists train_data, test_data, and new_data (empty)

functions_loc = "../TCcrediblebands/R/"
source(paste0(functions_loc, "read_data_source.R"))

# Read in train_names and test_names
load("data/train_test_names.Rdata")

# Pull data from HURDAT website
tc_list <- pull_data()

# Get training and test TC data
train_data <- tc_list[train_names]
test_data <- tc_list[test_names]
new_data <- list()

# Save .Rdata file with training data, test data, and empty new data
save(train_data, test_data, new_data, file = "data/raw_data.Rdata")
