# This file does the preprocessing for the spectral clustering visualization 

# current decision is to remove all excessive exploratory decision processes
# this includes: incorportating speed and location, and all LOOCV analysis 
# as this is just a visualization tool. We may include the files
# - initial_pipeline.R
# - loocv.R
# and 
# - second_pipeline.R
#
# in some type of "older_processing_code" code with some type of commenting 
# README.md file.

#############
# Libraries #
#############
# libraries, and functions 

project_location  <- ""
functions_loc     <- paste0(project_location,"code/functions/")
train_f_loc 	  <- paste0(project_location,"data/training/train/")
Rdata_files_train <- list.files(path = train_f_loc, pattern = ".Rdata")

desired_functions <- c("Path_functions.R","13pointreduction.R",
					   "projection_map.R","point_reduction_with_speed.R",
					   "estimating_p.R",
					   "load_training_first.R") 
					   # ^loading files function (clean up?)

for (f_name in desired_functions) {
  source(paste0(functions_loc,f_name))
}

######################
# Training Data Load #
######################

train_list <- load_train_first_data_list("")

####################
# Data Compression #
####################

if ("train_compression.Rdata" %in% Rdata_files_train) {
	load(paste0(train_f_loc,"train_compression.Rdata"))
}else{
	train13_12    <- compression_points_listable(list_df = train_list, 
											     lonlat = FALSE)
	train13       <- lapply(train13_12,function(l) l$new_13compression)
	train12_speed <- lapply(train13_12,function(l) l$new_12speed)
	save(train13,train12_speed,
		 file = paste0(train_f_loc,"train_compression.Rdata"))
}

###########################
# Lat/Lon Distance Matrix #
###########################

if ("D_matrix_squared_inner.Rdata" %in% Rdata_files_train) {
	load(file = paste0(train_f_loc,"D_matrix_squared_inner.Rdata"))
}else{
	D_train_squared <- distMatrixPath_innersq(path_mat_list = train13,
											  longlat = TRUE)
	save(D_train_squared,
			file = paste0(train_f_loc,"D_matrix_squared_inner.Rdata"))
}


####################
# P_train_creation #
####################

K = 4
t = 1
dim = 5

train_info = training_structure_estimating_p(D_train_squared, K, t, dim)

save(train_info,
	 file = paste0(train_f_loc,"spectral_clustering_structure.Rdata"))


