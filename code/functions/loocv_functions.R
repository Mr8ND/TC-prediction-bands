
functions_loc = "code/functions/"
desired_functions = c("projection_map.R","Path_functions.R")

# functions
for(f_name in desired_functions){
  source(paste0(functions_loc,f_name))
}


loocv_wrapper= function(path_mat_list,speed_list,Dmat,frac=1/2,
                        K=7,t=1,output_length="nautical mile",
                        longlat=TRUE,project_size = 4,plot_eigen =FALSE){
  # this function needs to:
  # 1) create a distance matrix for all observations (provided)
  # FOR EACH OBSERVATION:
  # a) Create transition matrix
  # b) decompose transition matrix
  # c) project to get new points (3d minus first observation vector - check plots of eigenvectors afterwards?)
  # d) get new distance structure for point
  # e) predict new point
  # f) save new points
  # g) calc distance between points
  

  n = length(path_mat_list) # 334
  storage = list() # holds new predicted points (path)
  storage_speed = list() #hold new predicted points (speed)
  diff = c()
  diff_speed = c()

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
    #phi_map_x_minus_obs = phi_map_x_minus_obs[,-1] #not needed for L2 norm (was needed for L1 norm)
    lambda_x_minus_obs = phi_map_out$lambda
    #lambda_x_minus_obs = lambda_x_minus_obs[-1]
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
                new_p_loc = obs_projection, K =K)
  
    point_speed = inverse_map(distance_projection =dist_minus_obs_project,
                projection_locs = minus_obs_projection,
                old_locs = speed_list[-obs],
                new_p_loc = obs_projection, K = K)  


  storage[[obs]] = point  
  storage_speed[[obs]] = point_speed
  diff[obs] = sum(distBetweenP(path_mat_list[[obs]],point)^2) 
  diff_speed[obs] = sum((speed_list[[obs]] - point_speed)^2)
  # note: when scaling remember to multiple by ratio_mean^2 when dealing with 
  # diffs (actually not since scaling was done to the squared versions, then
  # square rooted)
  }
    


  
  return(list(Dmat = Dmat, predicted_p = storage, diff_p = diff,
    predicted_s = storage_speed,diff_s = diff_speed,
    frac = frac,K =K,t=t))
}