# functions to project into the smaller space
# TODO: figure out why we have the knn function?
library(kknn)
library(geosphere)
library(ggplot2) #needed

#' Right Eigenvector Compression
#' 
#' Compresses P transitional matrix to approximate D^2_t matrix 
#' (diffusion distance at scale t)
#'
#' @param P transitional matrix
#' @param nu number of right eigenvectors
#' @param nv number of left eigenvectors
#' @param plot_n number of original eigenvectors to plot (if any)
#' @param t the number of steps you want to take with the transition matrix 
#' (power you raise the transition matrix)
#'
#' @return psi_map_x matrix (n x nv) of rows are \Psi_t(x)
#' @return lambda vector of original lambdas from the decomposition of P 
#' (length nv)
#' @export
#'
#' @examples
right_eigenvector_compression <- function(P, nu = 100, nv = 100, 
                                         plot_n = 0, t = 5){

  out <- svd(P, nu = nu,nv = nu) # need to make P centered as scaled?
  right_eigen <- out$v 
  # u ,v of svd are structured where u and v hold the columns 
  # are the singular vectors
  
  lambda_original <- out$d
  lambda <- lambda_original^t
  
  if (plot_n != 0) {
    ggplot() + 
      ggplot2::geom_bar(data = data.frame(y = lambda_original[1:plot_n]), 
               aes(x = 1:plot_n,y = y), stat = "identity")
  }
  
  Psi <- right_eigen
  psi_of_all_x <- Psi
  
  psi_map_of_all_x <- psi_of_all_x %*% diag(lambda[1:nv]) 

  return(list(psi_map_x = psi_map_of_all_x,lambda = lambda_original[1:nv]))
}

#' Project New Points with SCA
#'
#' creates the estimate of the projection of the test values into the Psi space
#' of training
#'
#' @param P_test transition matrix, t = 1, (n x m) calculated for the new points
#'  on the old points where n = number of new points (test), 
#'  m is number of old points (train)        
#' @param psi_map_train psi projection of training data (m x nv) where 
#' nv = dimension of projected space
#' @param lambda_train vector of lambdas of original decomposition of 
#' transition matrix (t=1), length = nv
#'
#' @return psi_map_estimated estimated psi projection of test data
#' @export
#'
#' @examples
new_points_projection <- function(P_test, psi_map_train, lambda_train){

  psi_over_lambda <- psi_map_train %*% diag(1/lambda_train) 
  psi_map_estimated <- P_test %*% psi_over_lambda
  
  return(psi_map_estimated)
}

#' KNN density estimate for each point 
#'
#' TODO: figure out why this isn't just knn...
#'
#' @param train 
#' @param test 
#' @param k number of neighbors that matter
#'
#' @return p density estimate for test observations 
#' @export
#'
#' @examples
kernel_estimate <- function(train,test,k){
  dists <- kknn::kknn.dist(train, test, k = k, distance = 3)    
  dists_desired <- dists[[2]][,k]
  
  d <- ncol(train)
  c_d <- pi^(d/2)/factorial(d/2 + 1)
  n <- nrow(train)
  
  p <- d/(n * c_d * dists_desired^d)
  
  return(list(p = p, dists_desired = dists_desired))
}









