# functions to project into the smaller space
library(kknn)
library(geosphere)

right_eigenvector_compression = function(P, nu = 100,nv = 100,plot_n=0,t=5){
  # Compresses P transitional matrix to approximate D^2_t matrix (diffusion distance at scale t)
  #
  # Inputs:
  # -------
  # P         = transitional matrix
  # nu, nv    = number of right and left eigen vectors (respectively)
  # plot_n    = if you want to have a plot display the "plot_n" number of eigenvalues make plot_n 
  #             the number of the largests eigenvalues you'd like plotted (original eigenvalues)
  # t         = the number of steps you want to take with the transition matrix
  #
  # Outputs:
  # --------
  # psi_map_x = matrix (n x nv) of rows are \Psi_t(x)
  # lambda    = original lambdas from the decomposition of P (length nv)
  
  out = svd(P, nu = nu,nv = nu) 
  right_eigen = out$v 
  # u ,v of svd are structured where u and v hold the columns are the singular vectors
  lambda_original = out$d
  lambda = lambda_original^t
  
  if (plot_n != 0){
    barplot(lambda_original[1:plot_n])
  }
  
  Psi = right_eigen
  
  psi_of_all_x = Psi
  psi_map_of_all_x = lambda[1:nv]* psi_of_all_x

  return(list(psi_map_x = psi_map_of_all_x,lambda = lambda_original[1:nv]))
}

new_points_projection = function(P_test,psi_map_train,lambda_train){
  # creates the estimate of the projection of the test values into the Psi space of training
  #
  # Inputs:
  # -------
  # P_test        = transition matrix,t = 1, (n x m) calculated for the new points on the old points
  #                   where n = number of new points (test), m is number of old points (train)        
  # psi_map_train = psi projection of training data (m x nv) where nv = dimension of projected space
  # lambda_train  = vector of lambdas of original decomposition of transition matrix (t=1), length = nv
  # 
  # Output:
  # -------
  # psi_map_estimated = estimated psi projection of test data
  
  psi_over_lambda = psi_map_train / lambda_train
  
  psi_map_estimated = P_test %*% psi_over_lambda
  
  return(psi_map_estimated)
}

kernel_estimate = function(train,test,k){
  # gives the KNN density estimate for each test point, given training and k
  #
  # Inputs: 
  #--------
  # train = training data (to create density estimate)
  # test  = test data, to get density estimate out for
  # k     = number of neighbors that matter
  #
  # Outputs:
  #---------
  # p     = density estimate for test observations 

  dists = kknn.dist(train, test, k = k, distance = 3)    
  dists_desired = dists[[2]][,k]
  
  d = ncol(train)
  c_d = pi^(d/2)/factorial(d/2+1)
  n = nrow(train)
  
  p  = d/(n*c_d*dists_desired^d)
  
  return(p)
}



loocv_wrapper= function(path_mat_list,Dmat,K=7,t=1,output_length="nautical mile",
                        longlat=TRUE,project_size = 5,plot_eigen =FALSE){
  # Inputs:
  #--------
  # path_mat_list = list of original paths of hurricanes
  # Dmat          = distances between hurricanes,
  #                    costly to compute if already made
  # K             = number of neigbhors for transition matrix creation
  # t             = power to raise transition matrix too
  # output_length = (not used), was for units of length outputted
  # longlat       = (not used), was logical statement if input was order longlat
  # project_size  = size of projection space
  # plot_eigen    = logical statement if function should plot eigenvalues,
  #                   for first LOOCV step only
  # 
  # Outputs
  #--------
  # Dmat          = returns Dmat put in (don't know why)
  # predicted     = predicted Hurricane paths 
  # diffs         = distance between predicted an true paths
  #
  # Notes:
  #-------
  # this function needs to:
  # 1) create a distance matrix for all observations  
  # FOR EACH OBSERVATION:
  # a) Create transition matrix
  # b) decompose transition matrix
  # c) project to get new points (3d minus first observation vector - 
  #    check plots of eigenvectors afterwards?)
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

    point = inverse_map(distance_projection = dist_minus_obs_project,
                projection_locs = minus_obs_projection,
                old_locs = path_mat_list[-obs], 
                new_p_loc = obs_projection,K =K)
  
      
  storage[[obs]] = point  
  diff[obs] = sum(distBetweenP(path_mat_list[[obs]],point))
  }
    


  
  return(list(Dmat = Dmat, predicted = storage, diffs = diff))
}







