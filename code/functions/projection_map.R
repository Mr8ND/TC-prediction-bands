# functions to project into the smaller space

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
    barplot(lambda_original[1:plot])
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







