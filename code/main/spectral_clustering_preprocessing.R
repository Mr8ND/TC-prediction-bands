#############
# Libraries #
#############
# libraries, and functions 

project_location  <- ""
functions_loc     <- paste0(project_location,"code/functions/")
data_loc 	  <- paste0(project_location,"data/")
Rdata_files_train <- list.files(path = train_f_loc, pattern = ".Rdata")

desired_functions <- c("Path_functions.R","13pointreduction.R",
					   "projection_map.R","point_reduction_with_speed.R",
					   "estimating_p.R") 

for (f_name in desired_functions) {
  source(paste0(functions_loc,f_name))
}

######################
# Training Data Load #
######################

training_list <- ...
load(paste0(data_loc,"loocv_optimal.Rdata"))
K = loocv_optimal["K"]
t = loocv_optimal["t"]
dim = 5

#####
# Create 13 point compression
#####

path_list_13_point <- thirteen_points_listable(training_list, # from 13pointreduction.R
                                               c_position = 5:6,
                                               lonlat = TRUE) # look at which columns to take

####
# Create Distance Matrix
####

Dmat <- distMatrixPath_innersq(path_list_13_point) # from Path_functions.R # make this function simplier and remove unneeded files

#####

train_info = training_structure_estimating_p(D_train_squared, K, t, dim)

save(train_info,
	 file = paste0(train_f_loc,"spectral_clustering_structure.Rdata"))


