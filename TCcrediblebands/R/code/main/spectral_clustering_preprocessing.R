# Libraries  -------------------------
# libraries, and functions 

project_location  <- ""
functions_loc     <- paste0(project_location,"code/functions/")
data_loc 	        <- paste0(project_location,"data/")

desired_functions <- c("path_functions.R","thirteen_point_compression.R",
					   "projection_map.R","point_reduction_with_speed.R",
					   "estimating_p.R") 

for (f_name in desired_functions) {
  source(paste0(functions_loc,f_name))
}

# Training Data Load -------------------------

load(paste0(data_loc,"raw_data.Rdata"))
load(paste0(data_loc,"loocv_optimal.Rdata"))
load(file = paste0(data_loc,"sca_training_structure.Rdata"))

K = loocv_optimal[["K"]]
t = loocv_optimal[["t"]]
dim = 5


# Fitting SCA with LOOCV based parameters -------------------------

train_info = training_structure_estimating_p(Dmat, K, t, dim)

save(train_info,
	 file = paste0(data_loc,"sca_model_structure.Rdata"))


