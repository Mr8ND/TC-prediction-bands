#############
# Libraries #
#############

library(geosphere)

#############
# Locations #
#############

project_location      = ""
functions_loc         = "code/functions/"
generate_loc          = "data/generate/"
train_f_loc           = "data/training/train/"

sub_directory_no_auto = paste0(generate_loc,"Validation_Sims_No_Auto/")
sub_directory_auto    = paste0(generate_loc,"Validation_Sims_Auto/")

desired_functions = c("Path_functions.R","13pointreduction.R",
  "projection_map.R","visualizing_color_gradient.R")

#####################
# Loading functions #
#####################

for(f_name in desired_functions){
  source(paste0(functions_loc,f_name))
}

# Ben's way to load data in:
source(paste0(functions_loc,"load_training_first.R"))


##########################
# Loading simulated data #
##########################

amount = 26:50
amount_string = "26_50"

#no_auto = load_sims(project_location,sub_directory_no_auto,group = amount)
auto = load_sims(project_location,sub_directory_auto,group = amount)

#files_no_auto = list.files(paste0(project_location,sub_directory_no_auto))
files_auto = list.files(paste0(project_location,sub_directory_auto))

file_names = unlist(strsplit(x = files_auto,split = "_ar_sims"))
file_names = as.vector(names)[amount]
########################
########################
# Algorithmic approach #
########################
########################

#################################
# Loading in training structure #
#################################

#train_list =load_train_first_data_list("")
#train13 =thirteen_points_listable(list_df = train_list,lonlat = FALSE)
#save(train13,file = paste0(train_f_loc,"train13.Rdata"))


#D_train = distMatrixPath(path_mat_list = train13,longlat = TRUE)
#  


load(paste0(train_f_loc,"train13.Rdata")) # named train13
load(paste0(train_f_loc,"D_matrix.Rdata")) # named D_train

test13_auto_list = list()
test13_no_auto_list = list()



for(i in amount){
  test13_auto_list[[i]] = 
    thirteen_points_listable(list_df = auto[[i]],
                             c_position = 1:2,lonlat = TRUE)
  #test13_no_auto_list[[i]] = 
  #  thirteen_points_listable(list_df = no_auto[[i]],
  #                           c_position = 1:2,lonlat = TRUE)
  print(i)
}

save(test13_auto_list,file = paste0(generate_loc,"test13_",amount_string,"_28nov_auto.Rdata"))
# load(paste0(generate_loc,"test13_first15_28nov.Rdata")) # test13_auto_list,test13_no_auto_list


D_test_auto_list = list()
D_test_no_auto_list = list()

for(i in amount){
  D_test_auto_list[[i]] = 
    distMatrixPath_train_test(path_mat_list_test = test13_auto_list[[i]],
                              path_mat_list_train = train13)
  
  #D_test_no_auto_list[[i]] = 
  #  distMatrixPath_train_test(path_mat_list_test = test13_no_auto_list[[i]],
  #                            path_mat_list_train = train13)
  print(i)
}

save(D_test_auto_list,file = paste0(generate_loc,"dmat_",amount_string,"_28nov_auto.Rdata"))
# load(paste0(generate_loc,"dmat_first5_27nov.Rdata")) # D_test_auto_list,D_test_no_auto_list


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


estimate_p_wrapper = function(training_structure_estimating_p,D_test,kdensity = 10){
  old_sigma_k     = training_structure_estimating_p$old_sigma_k
  P_train         = training_structure_estimating_p$P_train
  phi_map_out     = training_structure_estimating_p$phi_map_out
  phi_map_x_train = phi_map_out$psi_map_x
  lambda_x_train  = phi_map_out$lambda
  train_projected = training_structure_estimating_p$train_projected
  K               = training_structure_estimating_p$structure$K
  t               = training_structure_estimating_p$structure$t
  dim             = training_structure_estimating_p$structure$dim
  
  
  new_sigma_k = calc_k_dist(D_test,K=K)
  P_test      = probMatrixPath_k(D_test, old_sigma_k,new_sigma_k)
  
  test_projected= new_points_projection(P_test = P_test,psi_map_train = phi_map_x_train,lambda_train = lambda_x_train)
  test_projected = test_projected[,-1]
  
  p_estimate_test=kernel_estimate(train = train_projected,test = test_projected,k =kdensity) 
  
  return(list(new_sigma_k = new_sigma_k, P_test = P_test,test_projected= test_projected,p_estimate_test= p_estimate_test))
}








training_structure=training_structure_estimating_p(D_train,K=4,t=1,dim=5,plot_n = 0)
save(training_structure,file = paste0(train_f_loc,"training_structure.Rdata"))
#load(file = paste0(train_f_loc,"training_structure.Rdata")) # training_structure

#estimate_p_no_auto = list()
estimate_p_auto = list()

kdensity = 10

for(i in amount){
  #estimate_p_no_auto[[i]] = 
  #  estimate_p_wrapper(training_structure_estimating_p = training_structure,
  #                     D_test = D_test_no_auto_list[[i]],
  #                    kdensity = kdensity)
  estimate_p_auto[[i]]    = 
  estimate_p_wrapper(training_structure_estimating_p = training_structure,
                     D_test = D_test_auto_list[[i]],
                    kdensity = kdensity)
}

save(estimate_p_auto,file=paste0(generate_loc,"estimate_p_",amount_string,"_28nov_auto.Rdata"))

# validation_data=load_validate_data_list_names("",names =T)

# list_valid_subset = list()
# for(name in seq_along(file_names)){
#   list_valid_subset[[name]] = validation_data$list[[which(validation_data$names == paste0(file_names[name],".txt"))]]
# }

# list_valid_subset13 = thirteen_points_listable(list_df = list_valid_subset,lonlat = FALSE)


# for(i in seq_along(file_names)){
#   quartz()
#   plotting_funct(list_valid_subset13[[i]],list_estimate = test13_no_auto_list[[i]],weights= estimate_p_no_auto[[i]]$p_estimate_test,main= paste("No Auto",file_names[i]))
#   quartz()
#   plotting_funct(list_valid_subset13[[i]],list_estimate = test13_auto_list[[i]],weights= estimate_p_auto[[i]]$p_estimate_test,main= paste("Auto",file_names[i]))

# }





