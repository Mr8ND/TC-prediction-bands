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
    
    obs_column_matrix <- matrix(rep(obs_column, N - 1),nrow = N - 1)
    obs_row_matrix    <- matrix(rep(obs_row, N - 1),nrow = N - 1, byrow = T)
        
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