# Code to store names of train and test TCs in .Rdata file.
# Storing names will make train and test split reproducible.

library(TCpredictionbands)
library(splitstackshape)

# Make train and test names reproducible 
set.seed(1)

# Pull data from HURDAT website ----------------
tc_list <- pull_data()

# Set training and test proportions
train_prop <- 0.7
test_prop <- 1 - train_prop
  
# Select training TCs, stratified by year
df <- data.frame(names(tc_list), substr(names(tc_list), 5, 8))
colnames(df) <- c("name", "year")
train_sel <- stratified(df, group = "year", size = train_prop)

# Get training and test TC names
train_names <- as.character(train_sel$name)
test_names <- setdiff(df$name, train_sel$name)
  
# Save .Rdata file with train names and test names
save(train_names, test_names, file = "main/data/train_test_names.Rdata")

# Create training and test TC data lists
train_data <- tc_list[train_names]
test_data <- tc_list[test_names]
new_data <- list()

# Save .Rdata file with training data, test data, and empty new data
save(train_data, test_data, new_data, file = "main/data/raw_data.Rdata")
