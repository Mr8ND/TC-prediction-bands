library(progress)

functions_loc <- "code/functions/"
desired_functions <- c("projection_map.R","path_functions.R")

# functions
for (f_name in desired_functions) {
  source(paste0(functions_loc,f_name))
}


#' Calculates the LOOCV error rate for a specific local smoothing of distance 
#' $K$ and the power we raise the transition matrix to $t$
#'
#' @param path_mat_list list of paths to train on
#' @param Dmat distance matrix of the paths to train on
#' @param K K nearest neighbors value (for localization of distance matrix)
#' @param t power to raise the transition matrix P
#' @param output_length string of distance between points 
#' (set at "nautical mile" )
#' @param longlat boolean if order of columns is longlat
#' @param project_size dimension of projected space 
#' @param verbose it progress bar is provided
#'
#' @return Dmat distance matrix of the paths to train on that was inputted
#' @return predicted_curves list of predicted curves
#' @return diff_curves distance between true curves and predicted curves
#' @return K power to raise the transition matrix P (same as inputted)
#' @return t power to raise the transition matrix P (same as inputted)
#'
#' @examples
loocv_wrapper <- function(path_mat_list, Dmat,
                        K = 7, t = 1, 
                        output_length = "nautical mile",
                        longlat = TRUE, project_size = 4, 
                        verbose = TRUE){
  

  
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
  

  n <- length(path_mat_list) 
  storage_curve <- list() # holds new predicted path
  diff_curves <- c()

  # progress bar
  if (verbose) {
    pb <- progress_bar$new(
      format = "  Processing [:bar] :percent eta: :eta",
      total = n, clear = FALSE, width = 40)
  }
  
  
  for (obs in 1:n) {
    data_minus_obs   <- Dmat[-obs,-obs] 
    data_obs         <- matrix(Dmat[obs,-obs], nrow = 1) 
    dist_k_minus_obs <- calc_k_dist(data_minus_obs, K) 
    dist_k_obs       <- calc_k_dist(matrix(data_obs, nrow = 1), K)  
    
    prob_minus_obs <- probMatrixPath_k(data_minus_obs, 
                                       kNN_sigma = dist_k_minus_obs)

    # need to project into correct space
    phi_map_out <- right_eigenvector_compression(P = prob_minus_obs,
                                                 nu = project_size,
                                                 nv = project_size,
                                                 plot_n = 0, 
                                                 # ^ don't plot eigen values 
                                                 # in the LOOCV function 
                                                 t = t)
    phi_map_x_minus_obs <- phi_map_out$psi_map_x
    lambda_x_minus_obs <- phi_map_out$lambda

    # for new point
    prob_obs <- probMatrixPath_k(data_obs, kNN_sigma = dist_k_minus_obs,
                                 kNN_sigma_new = dist_k_obs)
    
    obs_projection <- new_points_projection(prob_obs,
                                            psi_map_train = phi_map_x_minus_obs,
                                            lambda_train = lambda_x_minus_obs)
    
    minus_obs_projection <- new_points_projection(prob_minus_obs,
                                            psi_map_train = phi_map_x_minus_obs,
                                            lambda_train = lambda_x_minus_obs)
    
    project_locs <- rbind(minus_obs_projection, obs_projection)
    project_dist <- distance_mat_eulid(project_locs)
    
    
    dist_minus_obs_project   <- project_dist[1:(n - 1), 1:(n - 1)]
    dist_obs_project         <- matrix(project_dist[n, 1:(n - 1)],nrow = 1)

     
    point <- inverse_map(distance_projection = dist_minus_obs_project,
                projection_locs = minus_obs_projection,
                old_locs = path_mat_list[-obs], 
                new_p_loc = obs_projection, K = K)


  storage_curve[[obs]] <- point  
  diff_curves[obs] <- sum(distBetweenP(path_mat_list[[obs]],point)^2) 
  
  if (verbose) {
    pb$tick()
  }
  
  }
    
  return(list(Dmat = Dmat, predicted_curves = storage_curve, 
              diff_curves = diff_curves, K = K, t = t))
}