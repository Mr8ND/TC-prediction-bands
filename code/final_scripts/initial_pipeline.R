# This file:
# 1) Loads in training data
# 2) Computes Distance between paths (both in locational and speed structure)
# 3) Weights the two and creates a D_combo matrix
# 4) Has a commented out example of visualizing the compression which goes into
#     both the D_combo and just the D_path

library(geosphere)


functions_loc = "code/functions/"
train_f_loc = "data/training/train/"
desired_functions = c("Path_functions.R","13pointreduction.R",
					"projection_map.R","point_reduction_with_speed.R")

# functions
for(f_name in desired_functions){
  source(paste0(functions_loc,f_name))
}

# Ben's way to load data in:
source(paste0(functions_loc,"load_training_first.R"))

train_list =load_train_first_data_list("")


Rdata_files_train = list.files(path = train_f_loc,pattern=".Rdata")


####################
# Data Compression #
####################

##train13 =thirteen_points_listable(list_df = train_list,lonlat = FALSE)
if("train_compression.Rdata" %in% Rdata_files_train){
	load(paste0(train_f_loc,"train_compression.Rdata"))
}else{
	train13_12 = compression_points_listable(list_df = train_list, lonlat = FALSE)
	train13 = lapply(train13_12,function(l) l$new_13compression)
	train12_speed = lapply(train13_12,function(l) l$new_12speed)
	save(train13,train12_speed,file=paste0(train_f_loc,"train_compression.Rdata"))
}

####################
# Lat/Lon Distance #
####################

#### Original Distance matrix, incorrectly programmed 
if("D_matrix.Rdata" %in% Rdata_files_train){
	load(paste0(train_f_loc,"D_matrix.Rdata"))
}else{
	D_train           = distMatrixPath(path_mat_list = train13,longlat = TRUE)
	save(D_train,file = paste0(train_f_loc,"D_matrix.Rdata"))
}


if("D_matrix_squared_inner.Rdata" %in% Rdata_files_train){
	load(file=paste0(train_f_loc,"D_matrix_squared_inner.Rdata"))
}else{
	D_train_squared = distMatrixPath_innersq(path_mat_list = train13,
														longlat = TRUE)
	save(D_train_squared,
			file=paste0(train_f_loc,"D_matrix_squared_inner.Rdata"))
}


##################
# Speed Distance #
##################

speed_df = matrix(unlist(train12_speed),nrow = length(train12_speed),byrow=T)

if("D_matrix_squared_speed.Rdata" %in% Rdata_files_train){
	load(file=paste0(train_f_loc,"D_matrix_squared_speed.Rdata"))
}else{
	D_train_squared_speed = distMatrixSpeed_innersq(speed_df = speed_df)
	save(D_train_squared_speed,
			file=paste0(train_f_loc,"D_matrix_squared_speed.Rdata"))
}


####################################################
# Weighting of Speed and Lat/Lon Distance Matrices #
####################################################


######################
# Ratios Exploration #
######################

vector_D_train_squared = c(D_train_squared)
vector_D_train_squared_speed = c(D_train_squared_speed)

ratio_max     = max(vector_D_train_squared)/max(vector_D_train_squared_speed)
ratio_mean    = mean(vector_D_train_squared)/mean(vector_D_train_squared_speed)
average_ratio = mean(c(ratio_max,ratio_mean)) 

################################
# Visualization of Exploration #
################################

# quartz(width = 10,height = 6)
# par(mfrow=c(1,3))

# # average_ratio
# r_mm=range(c(vector_D_train_squared_speed*average_ratio,vector_D_train_squared))
# breaks = seq(r_mm[1],r_mm[2]+1,length = 30)
# a = hist(vector_D_train_squared_speed*average_ratio,
# 	col=rgb(0,0,1,.5),breaks = breaks,main = "Average Ratio")
# b = hist(vector_D_train_squared,col=rgb(1,0,0,.5),
# 	breaks = breaks,add=T)


# # ratio_max
# r_mm=range(c(vector_D_train_squared_speed*ratio_max,vector_D_train_squared))
# breaks = seq(r_mm[1],r_mm[2]+1,length = 30)
# a = hist(vector_D_train_squared_speed*ratio_max,
# 	col=rgb(0,0,1,.5),breaks = breaks,main = "Ratio Max")
# b = hist(vector_D_train_squared,col=rgb(1,0,0,.5),
# 	breaks = breaks,add=T)


# # ratio_max
# r_mm=range(c(vector_D_train_squared_speed*ratio_mean,vector_D_train_squared))
# breaks = seq(r_mm[1],r_mm[2]+1,length = 30)
# a = hist(vector_D_train_squared_speed*ratio_mean,
# 	col=rgb(0,0,1,.5),breaks = breaks,main = "Ratio Mean")
# b = hist(vector_D_train_squared,col=rgb(1,0,0,.5),
# 	breaks = breaks,add=T)


############################
# Combining speed and path #
############################

## Notes: Based on these visuals, probably should go with "Ratio Means" 
## adjustment 

D_speed = D_train_squared_speed * ratio_mean
D_path  = D_train_squared 

D_combo = D_speed + D_path


if("D_combo.Rdata" %in% Rdata_files_train){
	load(paste0(train_f_loc,"D_combo.Rdata"))
}else{
	save(ratio_mean,D_combo,file = paste0(train_f_loc,"D_combo.Rdata"))
}

####################
# P_train_creation #
####################

K = 4
t = 1
dim = 5

training_structure_estimating_p = function(D_train,K=4,t=1,dim=5,plot_n = 0){
  old_sigma_k = calc_k_dist(D_train,K= K)
  P_train     = probMatrixPath_k(D_train, old_sigma_k)
  
  phi_map_out = right_eigenvector_compression(P = P_train,nu = dim,nv = dim,plot_n = plot_n,t = t)
  phi_map_x_train = phi_map_out$psi_map_x
  lambda_x_train = phi_map_out$lambda
  
  train_projected= new_points_projection(P_test = P_train,psi_map_train = phi_map_x_train,lambda_train = lambda_x_train)  
  train_projected = train_projected[,-1]
  
  return(list(old_sigma_k = old_sigma_k,P_train= P_train,
              phi_map_out = phi_map_out, train_projected = train_projected,
              structure = list(K= K,t=t,dim=dim)))
}
# ^ this function, along with others found in the second_pipeline should 
# probably be put in a function file

train_info = training_structure_estimating_p(D_combo,K,t,dim)


psi_map_x_train = train_info$phi_map_out["psi_map_x"][[1]]
lambda_x_train = train_info$phi_map_out["lambda"][[1]]

library(plot3D)
scatter3D(psi_map_x_train[,1],psi_map_x_train[,2],psi_map_x_train[,3],col=rgb(0,0,0,.3),pch=16)

################
# Testing Data #
################

# source(paste0(functions_loc,"load_training_first.R"))
# test_list = load_generated_curve_list("","Validation_Sims_Auto/AL021963_ar_sims/")

# test13_12 = compression_points_listable(list_df = test_list,c_position =1:2, 
# 											lonlat = TRUE)
# test13 = lapply(test13_12,function(l) l$new_13compression)
# test12_speed = lapply(test13_12,function(l) l$new_12speed)


# #D_test_path  = distMatrixPath_t2t_path(path_mat_list_train = train13,
# #										path_mat_list_test =test13)
# #save(D_test_path,file = "D_test_path.Rdata")
# load(file="D_test_path.Rdata")
# D_test_speed = distMatrixPath_t2t_speed(speed_mat_list_train=train12_speed,
# 										speed_mat_list_test=test12_speed)


# D_test_combo = sqrt(D_test_speed * ratio_mean + D_test_path)

# old_sigma_k  = train_info$old_sigma_k
# new_sigma_k  = calc_k_dist(D_test_combo,K=K)
# P_test       = probMatrixPath_k(D_test_combo, old_sigma_k,new_sigma_k)

# test_points_lowD = new_points_projection(P_test = P_test,
# 	psi_map_train = psi_map_x_train,lambda_train = lambda_x_train)

# quartz(width=10,height = 6)
# library(plot3D)

# par(mfrow=c(1,2))
# scatter3D(psi_map_x_train[,1],psi_map_x_train[,2],psi_map_x_train[,3],
# 	pch = 16, col=rgb(0,0,0,.3))
# points3D(test_points_lowD[,1],test_points_lowD[,2],test_points_lowD[,3],
# 	pch=16,col=rgb(1,0,0,.3),add=T)

# weights =kernel_estimate(train = psi_map_x_train,test = test_points_lowD,k =4)

# source(paste0(functions_loc,"visualizing_color_gradient.R"))

# validation_data=load_validate_data_list_names("",names =T)

# index_212003=grep("AL021963", validation_data$names)

# plotting_funct(validation_data$list[[index_212003]][,c(6,5)],
# 	list_estimate = test_list,
# 	weights= weights,
# 	main= paste("Auto Generated Curves"),
# 	lower="black",upper="pink",col="red")


# ####################
# # Just the lat/lon #
# ####################

# train_info_path = training_structure_estimating_p(sqrt(D_train_squared),K,t,dim)

# psi_map_x_train_path = train_info_path$phi_map_out["psi_map_x"][[1]]
# lambda_x_train_path = train_info_path$phi_map_out["lambda"][[1]]


# old_sigma_k_path  = train_info_path$old_sigma_k
# new_sigma_k_path  = calc_k_dist(sqrt(D_test_path),K=K)
# P_test_path       = probMatrixPath_k(sqrt(D_test_path), old_sigma_k_path,
# 										new_sigma_k_path)

# test_points_lowD_path = new_points_projection(P_test = P_test_path,
# 	psi_map_train = psi_map_x_train_path,lambda_train = lambda_x_train_path)

# quartz(width=10,height = 6)
# library(plot3D)

# par(mfrow=c(1,2))
# scatter3D(psi_map_x_train_path[,1],psi_map_x_train_path[,2],
# 	psi_map_x_train_path[,3],pch =16,col=rgb(0,0,0,.3))
# points3D(test_points_lowD_path[,1],test_points_lowD_path[,2],
# 	test_points_lowD_path[,3],pch =16,col=rgb(1,0,0,.3),add=T)

# weights =kernel_estimate(train = psi_map_x_train_path,test = test_points_lowD_path,k =4)


# index_212003=grep("AL021963", validation_data$names)

# plotting_funct(validation_data$list[[index_212003]][,c(6,5)],
# 	list_estimate = test_list,
# 	weights= weights,
# 	main= paste("Auto Generated Curves"),
# 	lower="black",upper="pink",col="red")
