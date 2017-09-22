library(tidyverse)

depth_function <- function(dist_matrix){
  # calcuates a depth vector using the distance matrix
  
  # expected a positive square symmetric matrix
  if(nrow(dist_matrix) != ncol(dist_matrix) | any(t(dist_matrix) != dist_matrix) | any(dist_matrix < 0)){
    print("this is not a positive symmetric square matrix")
    return(NA)
  }
  
  N = nrow(dist_matrix)
  N_step = as.integer(N/10)
  
  depth <- rep(0,N)
  
  for(obs_index in 1:N){
    sub_matrix <- dist_matrix[-obs_index,-obs_index]
    
    obs_column <- dist_matrix[-obs_index,obs_index]
    obs_row    <- dist_matrix[obs_index,-obs_index]
    
    obs_column_matrix <- matrix(rep(obs_column,N-1),nrow = N-1)
    obs_row_matrix    <- matrix(rep(obs_row,N-1),nrow = N-1, byrow = T)
    
    #^ does obs_column_matrix = t(obs_row_matrix) naturally?
    
    obs_combo_array <- array(0,dim = c(N-1,N-1,2))
    obs_combo_array[,,1] <- matrix(rep(obs_column,N-1),nrow = N-1)
    obs_combo_array[,,2] <- matrix(rep(obs_row,N-1),nrow = N-1, byrow = T)
    
    max_matrix <- sapply(1:(N-1), function(row_i) {
      sapply(1:(N-1), function(col_i) {
        max(obs_combo_array[row_i,col_i,1:2])
      })
      }) %>% t
    
    depth[obs_index] <- mean(sub_matrix > max_matrix)
    
    if(obs_index %% N_step == 0){
      cat(".")
    }
  }
  
  return(depth)
}


### test 
# set.seed(1)
# xx = sort(rnorm(100))
# dist_matrix<- as.matrix(dist(xx))
# 
# depth <- depth_function(dist_matrix = dist_matrix)
# 
# plot(xx,depth)
