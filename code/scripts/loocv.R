library(geosphere)
library(xtable)

functions_loc = "code/functions/"
train_f_loc = "data/training/train/"
desired_functions = c("Path_functions.R","13pointreduction.R","projection_map.R")

# functions
for(f_name in desired_functions){
  source(paste0(functions_loc,f_name))
}

load(paste0(train_f_loc,"D_matrix.Rdata"))
source(paste0(functions_loc,"load_training_first.R"))

train_list =load_train_first_data_list("")
train13 =thirteen_points_listable(list_df = train_list,lonlat = FALSE)

Dmat = D_train
path_mat_list  = train13

loocv_wrapper= function(path_mat_list,Dmat,K=7,t=1,output_length="nautical mile",
                        longlat=TRUE,project_size = 5,plot_eigen =FALSE){
  # this function needs to:
  # 1) create a distance matrix for all observations   (DONE)
  # FOR EACH OBSERVATION:
  # a) Create transition matrix
  # b) decompose transition matrix
  # c) project to get new points (3d minus first observation vector - check plots of eigenvectors afterwards?)
  # d) get new distance structure for point
  # e) predict new point
  # f) save new points
  # g) calc distance between points
  
  n = length(path_mat_list) # 334
  storage = list() # holds new predicted points
  diff = c()
  for(obs in 1:n){
    data_minus_obs   = Dmat[-obs,-obs] # 333 x 333
    data_obs         = matrix(Dmat[obs,-obs],nrow=1) # 1 x 333
    dist_k_minus_obs = calc_k_dist(data_minus_obs,K) # 333 x 1
    dist_k_obs       = calc_k_dist(matrix(data_obs,nrow=1),K) # 1 x 1 
    

    
    prob_minus_obs = probMatrixPath_k(data_minus_obs, kNN_sigma = dist_k_minus_obs)

    # need to project into correct space
    plot_n = 0
    if(obs ==1 & plot_eigen ==T){
      plot_n = project_size
    }
    
    phi_map_out = right_eigenvector_compression(
      P = prob_minus_obs,nu = project_size,nv = project_size,plot_n = plot_n,t = t)
    phi_map_x_minus_obs = phi_map_out$psi_map_x
    lambda_x_minus_obs = phi_map_out$lambda
    
    # for new point
    prob_obs = probMatrixPath_k(data_obs, kNN_sigma = dist_k_minus_obs,kNN_sigma_new = dist_k_obs)
    
    obs_projection = new_points_projection(prob_obs,psi_map_train = phi_map_x_minus_obs,
                          lambda_train = lambda_x_minus_obs)
    
    minus_obs_projection = new_points_projection(prob_minus_obs,
                                                 psi_map_train = phi_map_x_minus_obs,
                                                 lambda_train = lambda_x_minus_obs)
    
    project_locs = rbind(minus_obs_projection,obs_projection)
    project_dist = distance_mat_eulid(project_locs)
    
    
    dist_minus_obs_project   = project_dist[1:(n-1),1:(n-1)]
    dist_obs_project         = matrix(project_dist[n,1:(n-1)],nrow =1)
#   dist_k_minus_obs_project = calc_k_dist(data_minus_obs,K) # 333 x 1
#   dist_k_obs_project       = calc_k_dist(matrix(data_obs,nrow=1),K) # 1 x 1 
     
    point = inverse_map(distance_projection = dist_minus_obs_project,
                projection_locs = minus_obs_projection,
                old_locs = path_mat_list[-obs], 
                new_p_loc = obs_projection,K =K)
  
      
  storage[[obs]] = point  
  diff[obs] = sum(distBetweenP(path_mat_list[[obs]],point))
  }
    


  
  return(list(Dmat = Dmat, predicted = storage, diffs = diff))
}

again = F

if(again == T){
  capture = list()
  k_index = 1
  for(k in c(4,7,9,11)){
    print(k)
    capture_inner = list()
    t_index = 1
    for(t in c(1,3,5,7)){
      out =loocv_wrapper(path_mat_list,Dmat,K=k,t=t,output_length="nautical mile",
                     longlat=TRUE,project_size = 5,plot_eigen =FALSE)
      capture_inner[[t_index]] = out
      t_index = t_index + 1
    }
    capture[[k_index]] =capture_inner
    k_index = k_index + 1
    
    train_f_loc = "data/training/train/"
    save(capture,file = paste0(train_f_loc,"capture.Rdata"))
  }
}else{
    load(file = paste0(train_f_loc,"capture.Rdata"))
}


#####################################################
# Looking at L2 loss functions for varying K, and t #
#####################################################

error = matrix(0,nrow = 4, ncol=4)
for(i in 1:4){
  for(j in 1:4){
    error[i,j]= mean(capture[[i]][[j]]$diffs)
  }
}


colnames(error) = paste0("K= ", c(4,7,9,11),":")
rownames(error) = paste0("t= ",c(1,3,5,7),":")
# error
xtable(error)

############################
# plotting some estimates: #
############################

library(rworldmap)
newmap = getMap(resolution = "low")
plot(newmap, ylim = c(9, 40), xlim = c(-110, 2), asp = 1)

k4t1 = capture[[1]][[1]]
for(obs in 1:3){
lines(path_mat_list[[obs]][,1],path_mat_list[[obs]][,2],col="blue")
lines(k4t1$predicted[[obs]][,1],k4t1$predicted[[obs]][,2],col="red")
}

