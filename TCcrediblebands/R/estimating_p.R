functions_loc <- "R/"
desired_functions <- c("projection_map.R","Path_functions.R")

#' functions --------------
for (f_name in desired_functions) {
  source(paste0(functions_loc,f_name))
}



#' Creates structure for SCA algorithm from training data
#'
#' @param D_train Distance Matrix for training points
#' @param K K nearest neighbors value (for localization of distance matrix)
#' @param t power to raise the transition matrix P
#' @param dim dimension of projected space 
#' @param plot_n number of original eigenvectors to plot (if any)
#'
#' @return 
#' \item{old_sigma_k}{distance to Kth nearest neighbor for each training point}
#' \item{P_train}{transition matrix of training data after local scaling}
#' \item{phi_map_out}{right eigenvector decomposition taking into account K, t}
#' \item{train_projected}{data frame of training points in projected space}
#' \item{structure}{parameters list (k, t, dim from inputted parameters)}
#' @export 
training_structure_estimating_p <- function(D_train, K = 4, t = 1, dim = 5, 
                                            plot_n = 0){
  old_sigma_k <- calc_k_dist(D_train,K = K)
  P_train     <- probMatrixPath_k(D_train, old_sigma_k)
  
  phi_map_out <- right_eigenvector_compression(P = P_train,nu = dim,nv = dim,
                                               plot_n = plot_n,t = t)
  phi_map_x_train <- phi_map_out$psi_map_x
  lambda_x_train <- phi_map_out$lambda
  
  train_projected <- new_points_projection(P_test = P_train,
                                           psi_map_train = phi_map_x_train,
                                           lambda_train = lambda_x_train)  

  return(list(old_sigma_k = old_sigma_k, P_train = P_train,
              phi_map_out = phi_map_out, train_projected = train_projected,
              structure = list(K = K, t = t, dim = dim)))
}


#' Wrapper for getting structure of SCA lower dimensional projection
#'
#' @param training_structure_estimating_p list from 
#' training_structure_estimating_p
#' @param D_test Distance matrix between test points m and training points n
#' (a m x n matrix)
#' @param kdensity k for knn density caculation in projection space 
#'
#' @return 
#' \item{new_sigma_k}{distance to Kth neighbor for test set  (of points in 
#' training set), note this K is defined within training_structure_estimating_p 
#' parameter}
#' \item{P_test}{transition matrix after local scaling}
#' \item{test_projected}{data frame of projected test points}
#' \item{p_estimate_test}{vector of probability associated with each test point
#' (using kdensity parameter)}
#' @export
estimate_p_wrapper <- function(training_structure_estimating_p, D_test, 
                               kdensity = 10){
  old_sigma_k     <- training_structure_estimating_p$old_sigma_k
  P_train         <- training_structure_estimating_p$P_train
  phi_map_out     <- training_structure_estimating_p$phi_map_out
  phi_map_x_train <- phi_map_out$psi_map_x
  lambda_x_train  <- phi_map_out$lambda
  train_projected <- training_structure_estimating_p$train_projected
  K               <- training_structure_estimating_p$structure$K
  t               <- training_structure_estimating_p$structure$t
  dim             <- training_structure_estimating_p$structure$dim
  
  
  new_sigma_k <- calc_k_dist(D_test, K = K)
  P_test      <- probMatrixPath_k(D_test, old_sigma_k, new_sigma_k)
  
  test_projected <- new_points_projection(P_test = P_test,
                                          psi_map_train = phi_map_x_train,
                                          lambda_train = lambda_x_train)

  p_estimate_test <- kernel_estimate(train = train_projected,
                                     test = test_projected,
                                     k = kdensity) 
  
  return(list(new_sigma_k = new_sigma_k, P_test = P_test,
              test_projected = test_projected,
              p_estimate_test = p_estimate_test))
}
