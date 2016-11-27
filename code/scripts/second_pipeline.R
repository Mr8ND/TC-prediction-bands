#### second approach pipeline ####
##################################
# incorporates the new approach and uses the LOOCV tuned $K$ and $t$


library(geosphere)

functions_loc = "code/functions/"
train_f_loc = "data/training/train/"
desired_functions = c("Path_functions.R","13pointreduction.R","projection_map.R")

# functions
for(f_name in desired_functions){
  source(paste0(functions_loc,f_name))
}


########################################
# Loading Training and Small Test Data #
########################################
source(paste0(functions_loc,"load_training_first.R"))


####
# Training Data
#### 

train_list =load_train_first_data_list("")
train13 =thirteen_points_listable(list_df = train_list,lonlat = FALSE)
#D_train = distMatrixPath(path_mat_list = train13,longlat = TRUE)
#save(D_train,file = paste0(train_f_loc,"D_matrix.Rdata"))
load(paste0(train_f_loc,"D_matrix.Rdata")) # named D_train




####
# Small Test Data
#### 

test_list_big = load_generated_curve_list("","AL031985/")
test_list = list()
for(i in 1:150){
  test_list[[i]] = test_list_big[[i]]
}
test13 =thirteen_points_listable(list_df = test_list,c_position = 1:2,lonlat = TRUE)
D_test = distMatrixPath_train_test(path_mat_list_test = test13,path_mat_list_train = train13)


#########################################################
# Getting the probability structure and projection map: #
#########################################################

#Tuned parameters (except for dim)
K = 4
t = 1
dim = 5

old_sigma_k = calc_k_dist(D_train,K= K)
new_sigma_k = calc_k_dist(D_test,K=K)
P_train     = probMatrixPath_k(D_train, old_sigma_k)
P_test      = probMatrixPath_k(D_test, old_sigma_k,new_sigma_k)

phi_map_out = right_eigenvector_compression(P = P_train,nu = dim,nv = dim,plot_n = 0,t = t)
phi_map_x_train = phi_map_out$psi_map_x
lambda_x_train = phi_map_out$lambda

train_projected= new_points_projection(P_test = P_train,psi_map_train = phi_map_x_train,lambda_train = lambda_x_train)
test_projected= new_points_projection(P_test = P_test,psi_map_train = phi_map_x_train,lambda_train = lambda_x_train)

train_projected = train_projected[,-1]
test_projected = test_projected[,-1]

p_estimate_test=kernel_estimate(train = train_projected,test = test_projected,k =10) 
# ^ this K value doesn't need to be connected to the other K values

##################
# visualizations #
##################

# plot(p_estimate_test)
# library(plot3D)
# scatter3D(train_projected[,1],train_projected[,2],train_projected[,3],col=rgb(0,0,0,.5),pch=16)
# points3D(test_projected[,1],test_projected[,2],test_projected[,3],col=rgb(1,0,0,.5),add=T,pch=16)

# barplot(lambda_x_train)




