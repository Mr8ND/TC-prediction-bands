# Mathematical Inner Workings of SCA algorthm -----------------------



#' Right Eigenvector Compression
#' 
#' @description 
#' Compresses P transitional matrix to approximate \eqn{D^2_t} matrix 
#' (diffusion distance at scale t)
#'
#' @param P transitional matrix
#' @param nu number of right eigenvectors
#' @param nv number of left eigenvectors
#' @param plot_n number of original eigenvectors to plot (if any)
#' @param t the number of steps you want to take with the transition matrix 
#' (power you raise the transition matrix)
#'
#' @return 
#' \item{psi_map_x}{matrix (n x nv) of rows are \eqn{\Psi_t(x)}}
#' \item{lambda}{vector of original lambdas from the decomposition of P 
#' (length nv)}
#' @export
right_eigenvector_compression <- function(P, nu = 100, nv = 100, 
                                          plot_n = 0, t = 5){
  
  out <- svd(P, nu = nu,nv = nu) # need to make P centered as scaled?
  right_eigen <- out$v 
  # u ,v of svd are structured where u and v hold the columns 
  # are the singular vectors
  
  lambda_original <- out$d
  lambda <- lambda_original^t
  
  if (plot_n != 0) {
    ggplot2::ggplot() + 
      ggplot2::geom_bar(data = data.frame(y = lambda_original[1:plot_n],
                                          x = 1:plot_n), 
                        ggplot2::aes_string(x = 'x', y = 'y'), 
                        stat = "identity")
  }
  
  Psi <- right_eigen
  psi_of_all_x <- Psi
  
  psi_map_of_all_x <- psi_of_all_x %*% diag(lambda[1:nv]) 
  
  return(list(psi_map_x = psi_map_of_all_x,lambda = lambda_original[1:nv]))
}

#' Project New Points with SCA
#'
#' @description 
#' creates the estimate of the projection of the test values into the \eqn{\Psi}
#' space of training
#'
#' @param P_test transition matrix, \eqn{t = 1}, (n x m) calculated for the new 
#' points on the old points where n = number of new points (test), m is number 
#' of old points (train)        
#' @param psi_map_train psi projection of training data (m x nv) where 
#' nv = dimension of projected space
#' @param lambda_train vector of lambdas of original decomposition of 
#' transition matrix (\eqn{t = 1}), length is nv
#'
#' @return psi_map_estimated estimated psi projection of test data
#' @export
new_points_projection <- function(P_test, psi_map_train, lambda_train){
  
  psi_over_lambda <- psi_map_train %*% diag(1/lambda_train) 
  psi_map_estimated <- P_test %*% psi_over_lambda
  
  return(psi_map_estimated)
}

#' KNN density estimate for each point 
#'
#' @description 
#' KNN in Euclidean Distance
#'
#' @param train Training points
#' @param test Testing points
#' @param k number of neighbors that matter
#'
#' @return p density estimate for test observations 
#' @export
kernel_estimate <- function(train,test,k){
  dists <- kknn::kknn.dist(train, test, k = k, distance = 3)    
  dists_desired <- dists[[2]][,k]
  
  d <- ncol(train)
  c_d <- pi^(d/2)/factorial(d/2 + 1)
  n <- nrow(train)
  
  p <- d/(n * c_d * dists_desired^d)
  
  return(list(p = p, dists_desired = dists_desired))
}


# Wrapper Based Functions for SCA compression ----------------------
# including (1) setting up necessary structure to be applied multiple times, 
# (2) creation of projection of points, and (3) estimating probabilities in 
# low dimensional space, etc.



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
#' \code{\link{training_structure_estimating_p}}
#' @param D_test Distance matrix between test points m and training points n
#' (a m x n matrix)
#' @param kdensity k for knn density caculation in projection space 
#'
#' @return 
#' \item{new_sigma_k}{distance to Kth neighbor for test set  (of points in 
#' training set), note this K is defined within 
#' \code{\link{training_structure_estimating_p}} parameter}
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


#' Inner Function to apply SCA projection of a set of simulated curves and 
#' captures the associated probability values in the lower dimensional space
#'
#' @param test_data list of paths of test curves 
#' @param train_list_13_point list of 13 point compression of the paths of 
#' training curves, each in lonlat format for the first 2 columns
#' @param train_info structure for SCA algorithm from training data outputted
#' from \code{\link{training_structure_estimating_p}}.
#' @param c_position position of test_data's longitude and latitude columns 
#' respectively
#' @param verbose if TRUE each step of the process is expressed (1) 13 point 
#' compression, (2) creation of distance from test to training matrix, and 
#' (3) estimation of lower dimension projection and associated probabilites.
#'
#' @return 
#' \item{projection}{data frame of associated projected points of test / 
#' simulated curves}
#' \item{p_estimate}{probability estimates in the lower dimensional space of the
#' test / simulated curves}
#' @export
inner_sca_projection <- function(test_data, 
                                 train_list_13_point,
                                 train_info,
                                 c_position = 1:2, 
                                 verbose = TRUE){
  # calculating D_test --------------------
  
  test_list_13_point <- thirteen_points_listable(test_data, 
                                                 c_position = c_position,
                                                 lonlat = TRUE, 
                                                 verbose = verbose)
  # ^ from thirteen_point_compression.R
  
  D_test <- distMatrixPath_t2t_path(train_list_13_point, test_list_13_point, 
                                    output_length = "nautical mile", 
                                    longlat = TRUE, verbose = verbose)
  # ^ from path_functions.R
  
  # estimate P --------------------
  
  estimate_p_out <- estimate_p_wrapper(train_info, D_test, kdensity = 10) 
  # ^estimating_p.R
  test_projected  <- estimate_p_out$test_projected
  p_estimate_test <- estimate_p_out$p_estimate_test
  
  return(list(projection = test_projected, 
              p_estimate = p_estimate_test))
}    


#' Applies SCA projection of each set of simulated curves and captures the 
#' associated probability values in the lower dimensional space
#'
#' @description This function is a wrapper function of 
#' \code{\link{inner_sca_projection}}
#'
#' @param test_data_list a list of list of paths of test curves (simulated 
#' curves)
#' @param train_list_13_point list of 13 point compression of the paths of 
#' training curves, each in lonlat format for the first 2 columns
#' @param train_info structure for SCA algorithm from training data outputted
#' from \code{\link{training_structure_estimating_p}}.
#' @param c_position position of test_data's longitude and latitude columns 
#' respectively
#' @param verbose if TRUE, progress of processing of the each set of test curves
#' is expressed.
#' @param curve_type_vec vector with the strings referring to the different 
#' types of curves

#' @return A list information about each group of simulated curves, each element
#' is a list of the following components:
#' 
#' \item{projection}{data frame of associated projected points of test / 
#' simulated curves}
#' \item{p_estimate}{probability estimates in the lower dimensional space of the
#' test / simulated curves}
#' 
#' @export
sca_projection <- function(test_data_list, 
                           train_list_13_point,
                           train_info,
                           c_position = 1:2, 
                           verbose = TRUE,
                           curve_type_vec = c('Auto_DeathRegs', 
                                              'Auto_NoDeathRegs', 
                                              'NoAuto_DeathRegs', 
                                              'NoAuto_NoDeathRegs')){
  
  n_test <- length(test_data_list)
  k <- length(curve_type_vec)
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "SCA Projection [:bar] :percent eta: :eta",
      total = n_test*k, clear = FALSE, width = 44)
  }
  
  outlist <- list()
  
  if(is.null(names(test_data_list))){
    list_idx_vals = 1:n_test
  } else {
    list_idx_vals = names(test_data_list)
  }
  
  for (sim_paths_idx in list_idx_vals) {
    outlist[[sim_paths_idx]] <- list()
    for (curve_type in curve_type_vec) {
      outlist[[sim_paths_idx]][[curve_type]] <- inner_sca_projection(
        test_data_list[[sim_paths_idx]][[curve_type]],
        train_list_13_point,
        train_info,
        c_position = c_position, 
        verbose = FALSE)
      
      if (verbose) {
        pb$tick()
      }
    }
  }
  return(outlist)
} 


# Visualization tools -----------------------
# for the compression and weighting of curves



#' Spectral Clustering Process
#' 
#' @description 
#' produces weights and projection for paths (from projection)
#'
#' Note: This may be redudant now 
#' 
#' @param test_list list of paths to analysis (each a df)
#' @param train_df 13 point compression of training curves
#' @param D_train distance matrix from training curves
#' @param K nearest neighbors for spectral clustering inside smoothing
#' @param t power to raise spectral cluster transition matrix
#' @param dim dimension of lower dimensional projection from spectral c.
#' @param kdensity nearest neighbors for density estimate in lower dim space
#' @param c_position nearest neighbors for density estimate in lower dim space
#'
#' @return 
#' \item{test_p_estimate}{probability estimates for test data}
#' \item{test_weight}{scaled probabilities estimates (by max of probs) for test}
#' \item{test_13point}{13 point compression for test data}
#' \item{test_projected}{test data in projection space}
#' \item{train_projected}{training data in projection space}
#' 
#' @export
spectral_cluster_process <- function(test_list, train_df, D_train,
                                     K = 4, t = 1, dim = 5, kdensity = 10,
                                     c_position = 1:2){
  
  compression_both <- thirteen_points_listable(list_df = test_list,
                                               c_position = c_position,
                                               lonlat = TRUE)
  
  compression_pts <- lapply(compression_both, 
                            function(x){x$new_13compression})
  
  D_test <- distMatrixPath_t2t_path(
    path_mat_list_test = compression_pts,
    path_mat_list_train = train_df) 
  
  #^- should create matrix comparing training to ordered test matrix 
  # (13 point compression)
  # second_pipeline.R 121 stores a list for some reason
  
  training_structure <- training_structure_estimating_p(D_train,
                                                        K = 4,
                                                        t = 1,
                                                        dim = 5,
                                                        plot_n = 0)
  
  estimate_p <- estimate_p_wrapper(
    training_structure_estimating_p = training_structure,
    D_test = D_test,
    kdensity = kdensity)
  
  output <- list()
  output[["test_p_estimate"]] <- estimate_p$p_estimate_test$p
  output[["test_weight"]] <- estimate_p$p_estimate_test$p /
    max(estimate_p$p_estimate_test$p)
  output[["test_13point"]] <- compression_pts
  output[["test_projected"]] <- estimate_p$test_projected
  output[["train_projected"]] <- training_structure$train_projected
  
  return(output)
}




# ggplot wrappers --------------------------------------


#' Compresses data from list (to be plotted in ggplot)
#' 
#' @description 
#' Compresses data into tidyverse focused dataframe (for ggplot)
#'
#' @param test_list list of \eqn{s} paths to analysis (each a df)
#' @param sca_output a list of diffusion map associated information 
#' assumed of the form of output provided by 
#' \code{link{spectral_cluster_process}}. At minimum this list needs 2 elements
#' \itemize{
#' \item \code{test_p_estimate}: a \eqn{s} length vector of non-standardized 
#' probabilities (\eqn{p_i})
#' \item \code{test_weight}: a \eqn{s} length vector of standardized 
#' probabilities (\eqn{p_i/\max_k p_k})
#' }
#' @param c_position positions of lat and lon columns in test_list data frames
#'
#' @details 
#' \code{sca_output} is expected to be a list with at least 2 components: 
#' (1) \code{test_p_estimate} - probablity estimates of the test curves 
#' (\eqn{p_i})
#' (2) \code{test_weight} - a vector of standardized probabilities 
#' (\eqn{p_i/\max_k p_k})
#'
#' @return data frame that can be used to visualize curves
#' @export
#'
data_plot_sc_paths <- function(test_list, 
                               sca_output = 
                                 list(test_p_estimate = 
                                        rep(1,length(test_list)), 
                                      test_weight = 
                                        rep(1/length(test_list),
                                            length(test_list))), 
                               c_position = 1:2){
  
  data_out <- data.frame(lat = -360, long = -360, prob = 0, 
                         weight = 0, curve = 0)
  
  for (i in 1:length(test_list)) {
    data_out <- rbind(data_out,
                      data.frame(lat = test_list[[i]][,c_position[2]],
                                 long = test_list[[i]][,c_position[1]],
                                 prob = sca_output$test_p_estimate[i],
                                 weight = sca_output$test_weight[i],
                                 curve = i))
  }
  data_out <- data_out[-1,]
  
  return(data_out)
}


#' Visualize the TC paths with weights colored
#' 
#' @description 
#' Note / TODO: 
#' 1. Function currently uses geom_path - assumed euclidean space for map :/
#'
#' @param data_out data frame with correct path information same as outputed 
#' from data_plot_sc_paths functions. Specifically, columns as follows:
#' \itemize{
#' \item \code{curve} curve number indicating which curve the observation is in
#' \item \code{lat} latitude values for that step of the TC
#' \item \code{long} longitude values for that step of the TC
#' \item \code{weight} probability weights for each curve (between 0 and 1)
#' }
#' where we expect observations of the curve to ordered in time.
#' @param zoom map zoom for ggmap
#' @param test_color_power power to raise the probability weights 
#' (then use linear scaling for colors)
#' @param test_color_low lower color value for color range, 
#' @param test_color_high higher color value for color range,
#' @param n_breaks integer number of breaks along the color range
#' @param base_graph ggplot object for base graph 
#' (created from data_out otherwise)
#'
#' @return ggmap based map object
#' @export
#'
ggvis_paths_sca_weight <- function(data_out, zoom = 4,
                                   test_color_power = 1/3, 
                                   test_color_low = "white",
                                   test_color_high = "red",
                                   n_breaks = 10,
                                   base_graph = NULL){
  
  if (is.null(base_graph)) {
    latrange <- range(data_out$lat)
    lonrange <- range(data_out$long)
    
    ocean <- c(left = lonrange[1], bottom = latrange[1],
               right = lonrange[2], top = latrange[2])
    map   <- ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
    
    base_graph <- ggmap::ggmap(map)
    
  } 
  
  ## coloring
  color_out <- color_function(data_out$weight,
                              test_color_power = test_color_power,
                              test_color_low = test_color_low,
                              test_color_high = test_color_high,
                              n_breaks = n_breaks)
  
  data_out <- data_out %>% dplyr::mutate(weight_discrete = color_out$breaks)
  colors_rw <- color_out$colors_rw
  
  # final map:
  
  ggout <- base_graph + ggplot2::geom_path(data = data_out, 
                                  ggplot2::aes_string(x = 'long', y = 'lat', 
                                                      color = 'weight_discrete', 
                                                      group = 'curve')) +
    ggplot2::scale_color_manual(values = colors_rw) +
    ggplot2::labs(color = paste0("weights^(",round(test_color_power,2),")"))
  
  return(ggout)
}

#' Get points for objects in projection space
#'
#' @param sca_output spectral_cluster_process function output. Or at least the 
#' following structure in a list:
#' \itemize{
#' \item \code{test_projected} test data in projection space
#' \item \code{train_projected}training data in projection space
#' \item \code{test_p_estimate} probability estimates for test data (\eqn{p_i})
#' \item \code{test_weight} scaled probabilities estimates (by max of probs) for
#'  test  (\eqn{p_i/\max_k p_k})
#' }
#'
#' @return 
#' \item{test_df}{data frame of training points and probability weights}
#' \item{train_df}{data frame of test points}
#' @export
data_projection <- function(sca_output = list(test_projection = data.frame(),
                                              train_projected = data.frame(),
                                              test_p_estimate = integer(0),
                                              test_weight = integer(0))){
  n <- dim(sca_output$test_projected)[1]
  test_df <- sca_output$test_projected %>% data.frame %>% 
    dplyr::mutate(prob = sca_output$test_p_estimate,
                  weight = sca_output$test_weight,
                  curve = 1:n)
  train_df <- sca_output$train_projected %>% data.frame
  
  return(list(test_df = test_df, train_df = train_df))
}


#' color function setup
#'
#' @description 
#' creates a color ramp of \code{n_breaks} equally spaced (along a potentially
#' scaled version of) the inputted \code{weights_in}.
#' 
#'
#' @param weights_in standardized probability weights (between 0 and 1)
#' @param test_color_power power to raise the probability weights 
#' (then use linear scaling for colors)
#' @param test_color_low lower color value for color range, 
#' @param test_color_high higher color value for color range,
#' @param n_breaks integer number of breaks along the color range
#'
#' @return 
#' \item{breaks}{vector of which of the 10 breaks each weight is in 
#' (relative to power transformation)}
#' \item{colors_rw}{color palette ramp vector}
#'
#' @export
color_function <- function(weights_in,
                           test_color_power = 1/3,
                           test_color_low = "white",
                           test_color_high = "red", 
                           n_breaks = 10){
  
  w_func <- function(x){return(x^test_color_power)}
  
  breaks <- cut(w_func(weights_in), breaks = c(0, (1:n_breaks)/n_breaks))
  colors_rw <- grDevices::colorRampPalette(c(test_color_low, 
                                             test_color_high))(n_breaks)
  
  return(list(breaks = breaks, colors_rw = colors_rw))
}

#' Creates ggplot of projections
#'
#' @details 
#' ggplot experts are encouraged to use the output of data_projection 
#' function applied to sca_output instead of this wrapper
#'
#' @param sca_output spectral_cluster_process function output (see details)
#' @param train_alpha opacity level for black training points
#' @param test_color_power power transformation (x^ test_color_power) of 
#' probability values for test points color
#' @param test_color_low lower color for range of colors on test points prob
#' @param test_color_high high color for range of colors on test points prob
#' @param color_n_breaks integer number of breaks along the color range 
#' (equally spaced along transformed probability space)

#' @details 
#' \code{sca_output} is a list that, at minimum needs:
#' \itemize{
#' \item \code{test_projected} test data in projection space
#' \item \code{train_projected}training data in projection space
#' \item \code{test_p_estimate} probability estimates for test data (\eqn{p_i})
#' \item \code{test_weight} scaled probabilities estimates (by max of probs) for
#'  test (\eqn{p_i/\max_k p_k})
#' }
#'
#' @return ggplot scatter plot of training and colored test points
#' @export
#'
ggvis_projection <- function(sca_output, train_alpha = .3, 
                             test_color_power = 1/3, 
                             test_color_low = "white",
                             test_color_high = "red",
                             color_n_breaks = 10){
  # data
  data_p <- data_projection(sca_output)
  test_df <- data_p$test_df
  train_df <- data_p$train_df
  
  # color setup
  
  color_out <- color_function(test_df$weight,
                              test_color_power = test_color_power,
                              test_color_low = test_color_low,
                              test_color_high = test_color_high,
                              n_breaks = color_n_breaks)
  
  test_df <- test_df %>% dplyr::mutate(weight_discrete = color_out$breaks)
  colors_rw <- color_out$colors_rw
  
  
  ggout <- ggplot2::ggplot() +
    ggplot2::geom_point(data = train_df, 
                        ggplot2::aes_string(x = 'X1', y = 'X2'),
                        alpha = train_alpha, color = 'black') +
    ggplot2::geom_point(data = test_df, 
                        ggplot2::aes_string(x = 'X1', y = 'X2', 
                                            fill = 'weight_discrete'),
                        shape = 21, color = "black") +
    ggplot2::scale_fill_manual(values = colors_rw) + 
    ggplot2::labs(fill = paste0("weights^(",round(test_color_power,2),")"))
  
  
  return(ggout)
}




#' Creates clean visual of weighted curves using Spectral Clustering Analysis
#' 
#' @description
#' Creates clean visual of weighted curves using Spectral Clustering Analysis.
#' Specifically, the projected points in 2d and the paths colored
#'
#' @details 
#' ggplot experts are encouraged to use the output of this function to  
#' instead of just running with the created plot
#'
#' @param sca_output a list of diffusion map associated information 
#' assumed of the form of output provided by 
#' \code{link{spectral_cluster_process}}. At minimum this list needs 2 elements
#' \itemize{
#' \item \code{test_projected} test data in projection space
#' \item \code{train_projected}training data in projection space
#' \item \code{test_p_estimate} probability estimates for test data (\eqn{p_i})
#' \item \code{test_weight} scaled probabilities estimates (by max of probs) for
#'  test (\eqn{p_i/\max_k p_k})
#' }
#' @param test_list list of paths to analysis (each a df)
#' @param c_position positions of lat and lon columns in test_list data frames
#' @param zoom map zoom for ggmap (plot 2: map)
#' @param train_alpha opacity level for black training points (plot 1: scatter)
#' @param test_color_power power transformation (x^ test_color_power) of 
#' probability values for test points color (plot 1: scatter)
#' @param test_color_low lower color for range of colors on test points prob
#' (plot 1: scatter)
#' @param test_color_high high color for range of colors on test points prob
#' (plot 1: scatter)
#' @param color_n_breaks integer number of breaks along the color range 
#' (equally spaced along transformed probability space) (plot 1: scatter)
#' @param base_graph ggplot object for base graph 
#' (created from data_out otherwise) (plot 2: map)
#' @return 
#' \item{gg_path}{ggmap based map object of colored test curves}
#' \item{gg_proj}{ggplot scatter plot of training and colored test points}
#' also visualizes both graphics together using grid.arrange
#' @export
ggvis_all_weighted <- function(sca_output, test_list,
                               c_position = 1:2,
                               zoom = 4,
                               train_alpha = .3, 
                               test_color_power = 1/3, 
                               test_color_low = "white",
                               test_color_high = "red",
                               color_n_breaks = 10,
                               base_map_graph = NULL){
  # create data
  data_curves <- data_plot_sc_paths(test_list, sca_output, c_position)
  
  gg_path <- ggvis_paths_sca_weight(data_curves, zoom = zoom,
                                     test_color_power = test_color_power, 
                                     test_color_low = test_color_low,
                                     test_color_high = test_color_high,
                                     base_graph = base_map_graph)
  gg_proj <- ggvis_projection(sca_output, train_alpha = train_alpha, 
                              test_color_power = test_color_power, 
                              test_color_low = test_color_low,
                              test_color_high = test_color_high,
                              color_n_breaks)
  
  gridExtra::grid.arrange(gg_proj, gg_path, nrow = 1)
  
  return(list(gg_path = gg_path, gg_proj = gg_proj))
  
}
