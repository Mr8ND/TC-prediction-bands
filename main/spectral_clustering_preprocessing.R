# Locations  -------------------------
data_loc  <- "main/data/"

# Install from Github ----------------------------------

library(devtools)
devtools::install_github(repo = 'Mr8ND/Hurricanes_701/TCcrediblebands')
library(TCcrediblebands)

# Training Data Load -------------------------

load(paste0(data_loc,"raw_data.Rdata"))
load(paste0(data_loc,"loocv_optimal.Rdata"))
load(file = paste0(data_loc,"sca_training_structure.Rdata"))

K <- loocv_optimal[["K"]]
t <- loocv_optimal[["t"]]
dim <- 5

# Fitting SCA with LOOCV based parameters -------------------------

train_info <- training_structure_estimating_p(Dmat, K, t, dim)

save(train_info,
	 file = paste0(data_loc,"sca_model_structure.Rdata"))


