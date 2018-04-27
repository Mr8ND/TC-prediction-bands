# Code to store names of train and test TCs in .Rdata file.
# Storing names will make train and test split reproducible.

library(TCcrediblebands)
library(splitstackshape)

# Make train and test names reproducible
set.seed(10)

# Pull data from HURDAT website
tc_list <- pull_data()

# Set training and test proportions
train_prop <- 0.7
test_prop <- 1 - train_prop
  
# Select training TCs, stratified by year
df <- data.frame(names(tc_list), substr(names(tc_list), 5, 8))
colnames(df) <- c("name", "year")
train_sel <- stratified(df, group = "year", size = train_prop)


library(gridExtra)
grid.arrange(a,b,nrow = 2)

# Get training and test TC names
train_names <- as.character(train_sel$name)
test_names <- setdiff(df$name, train_sel$name)
  
# Save .Rdata file with train names and test names
save(train_names, test_names, file = "data/train_test_names.Rdata")
