#' Library --------------------------

library(tidyverse) # this library is needed here

#' Functions --------------------------

#' Depth calculation
#' 
#' @description
#' calculates a depth vector using a distance matrix
#'
#' @param dist_matrix  a n x n square positive symmetric matrix
#'
#' @return depth vector length n with depth values associated with indices in 
#' dist_matrix
#' @export
#'
#' @examples
depth_function <- function(dist_matrix){

  if (nrow(dist_matrix) != ncol(dist_matrix) | 
     any(t(dist_matrix) != dist_matrix) | 
     any(dist_matrix < 0)) {
    print("this is not a positive symmetric square matrix")
    return(NA)
  }
  
  N <- nrow(dist_matrix)
  N_step <- as.integer(N/10)
  
  depth <- rep(0,N)
  
  for (obs_index in 1:N) {
    sub_matrix <- dist_matrix[-obs_index,-obs_index]
    
    obs_column <- dist_matrix[-obs_index,obs_index]
    obs_row    <- dist_matrix[obs_index,-obs_index]
    
    obs_column_matrix <- matrix(rep(obs_column, N - 1), nrow = N - 1)
    obs_row_matrix    <- matrix(rep(obs_row, N - 1), nrow = N - 1, byrow = T)
        
    obs_combo_array <- array(0, dim = c(N - 1, N - 1, 2))
    obs_combo_array[,,1] <- matrix(rep(obs_column, N - 1), nrow = N - 1)
    obs_combo_array[,,2] <- matrix(rep(obs_row, N - 1), nrow = N - 1, byrow = T)
    
    max_matrix <- sapply(1:(N - 1), function(row_i) {
      sapply(1:(N - 1), function(col_i) {
        max(obs_combo_array[row_i, col_i, 1:2])
      })
      }) %>% t
    
    depth[obs_index] <- mean(sub_matrix > max_matrix)

  }
  
  return(depth)
}


#' Create a data frame of points in selected curves
#' 
#' NOTE: this functions is slightly redunant with a plotting function somewhere
#'
#' @param data_list list of hurricanes
#' @param desired_index which hurricanes to be included
#' @param verbose if progress is to be reported in creation of data frame
#'
#' @return data frame with points in desired curves
#' @export
#'
#' @examples
selected_paths_to_df <- function(data_list, desired_index = NULL, 
                                 verbose = TRUE){
  if (is.null(desired_index)) {
    desired_index = 1:length(data_list)
  }
  
  if (verbose) {
    n_desired = length(desired_index)
    pb <- progress::progress_bar$new(
      format = "Convert List to Data Frame [:bar] :percent eta: :eta",
      total = n_desired, clear = FALSE, width = 51)
  }

  data_list <- lapply(data_list, as.data.frame)
  df_out <- data_list[[1]][1,] %>% dplyr::mutate(curve = 0)
  
  for (good_curve_idx in desired_index) {
    df_out <- rbind(df_out, 
                    data_list[[good_curve_idx]] %>%
                      mutate(curve = good_curve_idx))
    if (verbose) {
      pb$tick()
    }
  }
  
  df_out <- df_out[-1,]
  
  return(df_out)
}




#' Get deepest curves' points in a data frame
#'
#' @param data_list list of hurricanes
#' @param alpha for credible band (related to depth)
#' @param dist_mat distance matrix (otherwise is calculated)
#' @param verbose if the distance matrix is verbose
#' @param c_position only needed if created 13 point reduction 
#' @param ... other parameters in distance calculation through 
#' `distMatrixPath_innersq`
#' @param depth_vector vector of depth values (otherwise calculated)
#'
#' @return data frame with points in desired curves
#' @export
#'
#' @examples
depth_curves_to_points <- function(data_list, alpha, dist_mat = NULL, 
                                   c_position = 1:2,
                                   depth_vector = NULL,
                                   verbose = FALSE, ...){
  if (is.null(depth_vector)){
    
    if (is.null(dist_mat)){
      # distance matrix ----------------
      dflist_13pointsreduction = thirteen_points_listable(dflist, 
                                                          c_position = c_position,
                                                          verbose = verbose)
      
      dist_mat = distMatrixPath_innersq(data_list, verbose = verbose, ...)
    }
    
    # depth approach ---------------
    depth_vector <- depth_function(dist_mat)
  }
  
  deep_idx <- which(depth_vector > quantile(depth_vector, probs = alpha))
  
  data_deep_df <- selected_paths_to_df(data_list, deep_idx, verbose = verbose)
  data_deep_points <- data_deep_df[, -which(names(data_deep_df) == "curve")]
  
  return(data_deep_points)
}



