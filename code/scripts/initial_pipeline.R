# loocv:  fitting epsilon and t
library(geosphere)


functions_loc = "code/functions/"
train_f_loc = "data/training/train/"
desired_functions = c("Path_functions.R","13pointreduction.R","projection_map.R")

# functions
for(f_name in desired_functions){
  source(paste0(functions_loc,f_name))
}

# Ben's way to load data in:
source(paste0(functions_loc,"load_training_first.R"))

train_list =load_train_first_data_list("")



train13 =thirteen_points_listable(list_df = train_list,lonlat = FALSE)

#D_train = distMatrixPath(path_mat_list = train13,longlat = TRUE)
#save(D_train,file = paste0(train_f_loc,"D_matrix.Rdata"))
load(paste0(train_f_loc,"D_matrix.Rdata"))
P_train = probMatrixPath(dist_matrix = D_train)


phi_map_out = right_eigenvector_compression(P = P,nu = 3,nv = 3,plot_n = ,t = 5)
phi_map_x_train = phi_map_out$psi_map_x
lambda_x_train = phi_map_out$lambda



# need to load in test points
# Ben's way to load data in:
source(paste0(functions_loc,"load_training_first.R"))
test_list = load_generated_curve_list("","AL021972/")


test13 =thirteen_points_listable(list_df = test_list,c_position = 1:2,lonlat = TRUE)


D_test = distMatrixPath_train_test(path_mat_list_test = test13,path_mat_list_train = train13)
P_test = probMatrixPath(dist_matrix = D_test)

test_points_lowD= new_points_projection(P_test = P_test,psi_map_train = phi_map_x_train,lambda_train = lambda_x_train)

library(plot3D)
scatter3D(phi_map_x_train[,1],phi_map_x_train[,2],phi_map_x_train[,3],col="black")
scatter3D(hold[,1],hold[,2],hold[,3],col="black")

#points3D(test_points_lowD[,1],test_points_lowD[,2],test_points_lowD[,3],col="red",add=T)

a=kernel_estimate(train = phi_map_x_train,test = test_points_lowD,k =4)
