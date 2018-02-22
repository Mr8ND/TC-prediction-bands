library(geosphere)
library(datamart)
library(progress)
#' Swap the 2 columns in a Data Frame
#'
#' NOTE: not sure this should be exported
#'
#' @param data_df data frame (assumed to have 2 columns)
#'
#' @return data frame with columns reverse (actually just first 2 columns 
#' included)
#' @export
#'
#' @examples
swap2DfCols <- function(data_df){
	return(data.frame(cbind(data_df[, 2], data_df[, 1])))
}

#' Calculates the distance and bearing between each point of a path.
#'
#' @param data_df (n x 2) data.frame, each row is a pair of values (lat, lon) 
#' unless specified the reverse with longlat boolean
#' @param output_length string of measurement unit of points, see options in
#' datamart::uconv.
#' @param longlat boolean if the order of the columns is longitude then latitude
#' - if FALSE then column order is reversed.
#' @param verbose boolean logic if should have print outs (specifically that we
#' switch the columns due to longlat boolean)
#'
#' @return distance vector of distances beween points (n - 1)
#' @return bearing vector of bear change between points (n - 1)
#' @export
#'
#' @examples
distAlongP <- function(data_df, output_length = "nautical mile", 
                       longlat = TRUE, verbose = FALSE){
  
	if (longlat == FALSE) {
	  if (verbose) {
		  print("-- Formatting the df in longitude - latitude form --")
	  }
	  data_df <- swap2DfCols(data_df)
	}

	n <- dim(data_df)[1]
	output_d <- numeric(n - 1)
	output_b <- numeric(n - 1)

	for (i in c(1:(n - 1))) {
		output_d[i] <- datamart::uconv(geosphere::distGeo(data_df[i, 1:2], 
		                                                  data_df[i + 1, 1:2]),
		                     "m", output_length, "Length")
		output_b[i] <- bearing(data_df[i, 1:2], data_df[i + 1, 1:2])
	}

	return(list(distance = output_d, bearing = output_b))
}


#' Calculates the distance between each point of every path.
#'
#' @param data_df_p1 (n x 2) data.frame, each row is a pair of values (lat, lon) 
#' unless specified the reverse with longlat boolean
#' @param data_df_p2 (n x 2) second data.frame to be compared, each row is a 
#' pair of values (lat, lon) unless specified the reverse with longlat boolean
#' @param output_length string of measurement unit of points, see options in
#' datamart::uconv.
#' @param longlat boolean if the order of the columns is longitude then latitude
#' - if FALSE then column order is reversed.
#' @param verbose boolean logic if should have print outs (specifically that we
#' switch the columns due to longlat boolean)
#'
#' @return vector of n distances between each order point in the two data frames
#' @export
#'
#' @examples
distBetweenP <- function(data_df_p1, data_df_p2, 
                        output_length = "nautical mile", 
                        longlat = TRUE, verbose = FALSE){
	if (longlat == FALSE) {
	  if (verbose) {
		  print("-- Formatting the dfs in longitude - latitude form --")
	  }
	  data_df_p1 <- swap2DfCols(data_df_p1)
	  data_df_p2 <- swap2DfCols(data_df_p2)
	}

	n = dim(data_df_p1)[1]
	if (n != dim(data_df_p2)[1]) {
		stop("DFs have different dimension")
	}
	output_d = numeric(n - 1)

	for (i in c(1:n)) {
		output_d[i] <- datamart::uconv(geosphere::distGeo(data_df_p1[i,], 
		                                                 data_df_p2[i,]),
		                              "m", output_length, "Length")
	}

	return(output_d)
}


#' #' calculates the distance matrix Delta between n paths
#' #'
#' #' TODO: I'm confused why the diagonal should be / is 1? - Nic? 
#' #' when I run this (in your credible_interval_pipeline) I get 0.
#' #' 
#' #' TODO: this function is outdated. Should be removed.
#' #'
#' #' @param path_mat_list 
#' #' @param output_length 
#' #' @param longlat 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' distMatrixPath <- function(path_mat_list, output_length = "nautical mile", 
#'                            longlat = TRUE){
#' 	#This function calculates the distance matrix Delta between n paths.
#' 	#This matrix is a symmetric matrix, with the diagonal being 1, and each of the off
#' 	#diagonal element is the sum of the absolute distance of each point of the two paths.
#' 
#' 	#Input is a list of path. Each path is a matrix of (k x 2) elements, in which each
#' 	#row is a pair of longitude latitude points.
#' 	#Default is that the pair is (longitude, latitude), which is the flag variable
#' 	#"longlat". If this is set to false then both the dataframes will be simply swapped.
#' 
#' 	#Ouput is the delta matrix distance, output as a matrix (n x n).
#' 
#' 	n_mat <- length(path_mat_list)
#' 	output_mat <- matrix(, nrow = n_mat, ncol = n_mat)
#' 
#' 	for (i in c(1:n_mat)) {
#' 		for (j in c(i:n_mat)) {
#' 			output_mat[i,j] <- sum(distBetweenP(path_mat_list[[i]], 
#' 			                                   path_mat_list[[j]],
#' 			                                   longlat = longlat))
#' 			output_mat[j,i] <- output_mat[i,j]
#' 		}
#' 	}
#' 
#' 	return(output_mat)
#' }
# 
# distMatrixPath_train_test = function(path_mat_list_train,path_mat_list_test, output_length="nautical mile", longlat= TRUE){
#   #This function calculates the distance matrix Delta between n paths.
#   #This matrix is a symmetric matrix, with the diagonal being 1, and each of the off
#   #diagonal element is the sum of the absolute distance of each point of the two paths.
#   
#   #Input is a list of path for training data and a list of path for test data. 
#   # Each path is a matrix of (k x 2) elements, in which each
#   #row is a pair of longitude latitude points.
#   #Default is that the pair is (longitude, latitude), which is the flag variable
#   #"longlat". If this is set to false then both the dataframes will be simply swapped.
#   # Need both groups to be in the same order
#   
#   #Ouput is the delta matrix distance, output as a matrix (m x n). m test,n training paths
#   
#   n_mat = length(path_mat_list_test)
#   m_mat = length(path_mat_list_train)
#   output_mat = matrix(, nrow=n_mat, ncol=m_mat)
#   
#   for (i in c(1:n_mat)){
#     for (j in c(1:m_mat)){
#       output_mat[i,j] = sum(distBetweenP(path_mat_list_test[[i]], path_mat_list_train[[j]],longlat=longlat))
#     }
#   }
#   
#   return(output_mat)
# }

# 
# probMatrixPath = function(dist_matrix, epsilon = 430){
# 	#Given a certain distance Matrix D, this function gives back the probability matrix
# 	#indicated in the paper, by first trasforming it using the exp(-D/epsilon).
# 	#Epsilon is by default 430, but it can be changed using the related variable.
# 
# 	#The input is a matrix (n x n).
# 	#Ouput is a matrix (n x n), with the rows summing to one.
# 
# 	w = exp(-1*(dist_matrix/epsilon))
# 	return(w/rowSums(w))
# }

#####################################
# Extensions to probMatrix Creation #
#####################################

#' Finds the kth smallest distance for each point
#'
#' @param dist_matrix n x k* matrix with distances (each row an observation)
#' k* needs to be greater than k 
#' @param K number of neighbors desired
#'
#' @return dist_k vector with kth distance (per each row)
#' @export
#'
#' @examples
calc_k_dist <- function(dist_matrix, K = 7){
  ncol_D <- ncol(dist_matrix)
  
  if (ncol_D < K) {
    stop("distance matrix is too small for that size of K")
  }
  dist_k <- sqrt(apply(dist_matrix, 1, function(row) sort(row)[K])) 
  # ^ sqrt to make a distance (may be wrong)
  
  return(dist_k)
}
  
#' Title Creates Probability matrix, using KNN vector ot hold sigma
#'
#' @param dist_matrix n x m distance matrix
#' @param kNN_sigma m x 1 vector with sigma estimates for original structure
#' @param kNN_sigma_new n x 1 vector with sigma estimates for new structure
#' if dist_matrix is not symmetric, assumed new relationships are being provided
#'
#' @return p n x m matrix (transitional matrix)
#' @export
#'
#' @examples
probMatrixPath_k = function(dist_matrix, kNN_sigma, kNN_sigma_new = kNN_sigma){

  if (!(length(kNN_sigma_new) == nrow(dist_matrix) & 
        length(kNN_sigma) == ncol(dist_matrix) )) {
    stop("Error in dimensions of inputs")
  }
      
  epsilon_matrix <- kNN_sigma_new %*% t(kNN_sigma)
  w <- exp(-1 * (dist_matrix / epsilon_matrix))
  p <- w / rowSums(w)
  return(p)
}


##########
##########

#' 
#' #' Given a certain probability matrix P, this function computes the matrix 
#' #' D^2(x,y) defines as follows:
#' #'   D^2(x,y) = sum_k (P(x,k)-P(y,k))/stat_k
#' #' Where stat_k is the stationary vector of the probability matrix.
#' #'
#' #' NOTE: This function always assumes that the probability matrix is built 
#' #' correctly and that all the rows sums to one, such that an eigenvalue 1 always
#' #' exists - and it is the biggest of the series. So the function goes and looks
#' #' for the first eigenvector of the series.
#' #' 
#' #'
#' #' @param prob_matrix probability/transition matrix (n x n)
#' #' @param t power to raise the transition
#' #'
#' #' @return D^2(x,y) matrix (n x n)
#' #' @export
#' #'
#' #' @examples
#' d2MatrixPath <- function(prob_matrix, t = 5){
#' 	prob_mat_t <- prob_matrix**t
#' 	eigenvec_1 <- eigen(prob_matrix)$vectors[,1]
#' 
#' 	n_mat <- dim(prob_mat_t)[1]
#' 	output_mat <- matrix(, nrow = n_mat, ncol = n_mat)
#' 
#' 	for (i in c(1:n_mat)) {
#' 		for (j in c(i:n_mat)) {
#' 			d2_vec <- (prob_matrix[i,] - prob_matrix[j,])/(eigenvec_1**(1/2))
#' 			output_mat[i,j] <- as.numeric(d2_vec %*% d2_vec)
#' 			output_mat[j,i] <- output_mat[i ,j] 
#' 		}
#' 	}
#' 
#' 	return(output_mat)
#' }

#######################################
# Extensions to d2MatrixPath Creation #
#######################################
# Updates to number of steps along transition matrix, averaging
# Now requires 2 functions 
# (I don't think we use the d2 but it wasn't the hard to copy)

#' Averages together P matrices raised to different t values
#'
#' Notes: Assumes that the P matrix is well created (rows sum to 1)
#'
#' The author knows longer knows what this does.
#' 
#' @param prob_matrix (n x n) transition matrix 
#' @param t_var integer or vector containing information about t
#' @param t_type string, either "upper", "vec", or "log2" 
#' "upper" : integer t_var, range t from 1:t_var
#' "vec"   : vector t_var range t along vec
#' "log2"  : integer t_var, range t from 2^(0-> t_var)
#'
#' @return p_average (n x n) transition matrix
#' @export
#'
#' @examples
pMatrixPath_average = function(prob_matrix, t_var, t_type = "upper"){
  
  # log2 way to express t ---------------------
  if (t_type == "log2") {
    p_average <- prob_matrix
    intermediate_pt <- p_average
    for (i in 1:t_var) {
      intermediate_pt <- intermediate_pt**2
      p_average <- p_average + intermediate_pt
    }
    p_average <- p_average / (t_var + 1)
    return(p_average)
  }
  
  # other ways to express t ---------------------
  if (t_type == "upper") {
    t_var <- 1:t_var
  }
  p_average <- matrix(0,nrow = nrow(prob_matrix),ncol = ncol(prob_matrix))
  
  for (i in t_var) {
    p_average <- p_average + prob_matrix**i
  }
  
  p_average <- p_average / length(t_var)
  return(p_average) 
}


#' Computes matrix D^2(x,y)
#' defines as follows:
#'    D^2(x,y) = sum_k (P(x,k)-P(y,k))/stat_k
#' Where stat_k is the stationary vector of the probability matrix.
#'
#' Note: 
#' This function always assumes that the probability matrix is built correctly
#' and that all the rows sums to one, such that an eigenvalue 1 always exists - 
#' and it is the biggest of the series. So the function goes and looks for the 
#' first eigenvector of the series.
#'
#' The author knows longer knows what this does.
#'
#' @param prob_matrix_t (n x n) probability matrix
#'
#' @return output_mat (n x n) D^2(x,y) matrix
#' @export
#'
#' @examples
d2MatrixPath_pt = function(prob_matrix_t){
  
  eigenvec_1 <- eigen(prob_matrix)$vectors[,1]
  
  n_mat <- dim(prob_mat_t)[1]
  output_mat <- matrix(NA, nrow = n_mat, ncol = n_mat)
  
  for (i in c(1:n_mat)) {
    for (j in c(i:n_mat)) {
      d2_vec <- (prob_matrix[i,] - prob_matrix[j,])/(eigenvec_1**(1/2))
      output_mat[i,j] <- as.numeric(d2_vec %*% d2_vec)
      output_mat[j,i] <- output_mat[i,j] 
    }
  }
  
  return(output_mat)
}


###########
###########

###################
# inverse mapping #
###################

# for a single observation: 

#' Wrapper for calculating euclidean distance, for a single TC
#'
#' @param projection_locs locations of TC in projected space (n x d)
#'
#' @return euclidean distance matrix comparing between points
#' @export
#'
#' @examples
distance_mat_eulid <- function(projection_locs){
  
  n <- nrow(projection_locs)
  dist_structure <- stats::dist(projection_locs, upper = TRUE, diag = TRUE)

  e_distance <- matrix(0, nrow = n, ncol = n)
  e_distance[lower.tri(x = e_distance,diag = F)] <- matrix(dist_structure)
  e_distance <- e_distance + t(e_distance)
  return(e_distance)
}



#' Predicts the Hurricane path from training data and location of projection
#'
#' This function does a lot, it:
#' 1) gets a prediction for s_i(k) for the new observation in the projection 
#' space
#' 2) uses the kNN approach to calculate closeness weights (scaled obviously)
#' 3) predicts the original space point by weighting the observations that it 
#' has seen
#'
#' @param distance_projection distances between observations in projection space 
#'							(euclidean structure assumed)
#' @param projection_locs projection location of training data
#' @param old_locs true paths (list) of training data
#' @param new_p_loc new observation's location in projection (p) space
#' @param K Number of neigbhors used to estimate \sigma_i(K)
#'
#' @return predicted path of new observation
#' @export
#'
#' @examples
inverse_map <- function(distance_projection, projection_locs,
                        old_locs, new_p_loc, K = 7){

  sigma_vec = calc_k_dist(distance_projection,K = K)
  
  # first get distance in between -------------------------
  # (using euclidean distance on projection space)
  distance <- sqrt(rowSums(t(((t(projection_locs) - as.vector(new_p_loc))**2))))
  
  # similar calc_k_dist function, but for 1 observation-------------------------
  sigma_k <- sort(distance)[K]
  
  weights <- exp(-distance**2/(sigma_k*sigma_vec))
  weights <- weights/sum(weights)
  
  predict <- matrix(0,nrow = nrow(old_locs[[1]]), ncol = ncol(old_locs[[1]]))

  for (i in 1:length(old_locs)) {
  	predict <- predict + old_locs[[i]] * weights[i]
  }

  return(predict)
}










##############################################
# Distance Between Speed and GeoDist Correct #
##############################################

#############################
# Train to Train (Standard) #
#############################

#' Calculates the distance matrix Delta between n paths
#'
#' @param path_mat_list list of paths (matricies/ dataframes)
#' @param output_length string of measurement unit of points, see options in
#' datamart::uconv.
#' @param longlat boolean if the order of the columns is longitude then latitude
#' - if FALSE then column order is reversed.
#' @param verbose boolean logic if should have print outs while computing 
#' distance matrix
#'
#' @return
#' @export
#'
#' @examples
distMatrixPath_innersq = function(path_mat_list, 
                                  output_length = "nautical mile", 
                                  longlat = TRUE,
                                  verbose = TRUE){
  n_mat <- length(path_mat_list)
  
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Creating Distance Matrix [:bar] :percent eta: :eta",
      total = ((n_mat)*(n_mat + 1))/2, clear = FALSE, width = 51)
  }

  output_mat <- matrix(0, nrow = n_mat, ncol = n_mat)

  for (i in c(1:n_mat)) {
    for (j in c(i:n_mat)) {
      output_mat[i,j] <- sum(distBetweenP(path_mat_list[[i]], 
                                         path_mat_list[[j]],
                                         longlat = longlat)^2)
      output_mat[j,i] <- output_mat[i,j]
      
      if (verbose) {
        pb$tick()
      }
    }
  }

  return(output_mat)
}



# distMatrixSpeed_innersq <- function(speed_df){
#   # this function is similar to the ones above - but we get the sum of square 
#   # differences of the speed (this ends up being just a wrapper)
#   #
#   #
# 
#   distance = dist(speed_df)
#   d2 = as.matrix(distance)^2
# 
#   return(d2)
# }



#######################
# Train to Test (t2t) #
#######################




#   #This function calculates the distance matrix Delta between n paths.
#   #This matrix is a symmetric matrix, with the diagonal being 1, and each of the off
#   #diagonal element is the sum of the absolute distance of each point of the two paths.
#   
#   #Input is a list of path for training data and a list of path for test data. 
#   # Each path is a matrix of (k x 2) elements, in which each
#   #row is a pair of longitude latitude points.
#   #Default is that the pair is (longitude, latitude), which is the flag variable
#   #"longlat". If this is set to false then both the dataframes will be simply swapped.
#   # Need both groups to be in the same order
#   
#   #Ouput is the delta matrix distance, output as a matrix (m x n). m test,n training paths
#   

#' Creates a path distance matrix between the test paths and training paths 
#' (t2t) contains the correct L2 norm structure sum of square distances
#'
#' @param path_mat_list_train list of paths of training curves (n objects)
#' @param path_mat_list_test list of paths of test curves (m objects)
#' @param output_length string of measurement unit of points, see options in
#' datamart::uconv.
#' @param longlat boolean if the order of the columns is longitude then latitude
#' - if FALSE then column order is reversed.
#' @param verbose boolean logic if should have print outs while computing 
#' distance matrix
#'
#' @return delta matrix distance, output as a matrix (m x n)
#' @export
#'
#' @examples
distMatrixPath_t2t_path <- function(path_mat_list_train, path_mat_list_test, 
                                   output_length = "nautical mile", 
                                   longlat = TRUE, verbose = FALSE){

  n_test  <- length(path_mat_list_test)
  n_train <- length(path_mat_list_train)
  output_mat <- matrix(NA, nrow = n_test, ncol = n_train)
  
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Creating Distance Matrix [:bar] :percent eta: :eta",
      total = n_test*n_train, clear = FALSE, width = 51)
  }
  

  
  for (i in c(1:n_test)) {
    for (j in c(1:n_train)) {
      output_mat[i,j] <- sum(distBetweenP(path_mat_list_test[[i]], 
                                          path_mat_list_train[[j]],
                                          longlat = longlat,
                                          output_length = "nautical mile",
                                          verbose = FALSE)^2)
      if (verbose) {
        pb$tick()
      }
    }
  }
  
  return(output_mat)
}






# 
# distMatrixPath_t2t_speed = function(speed_mat_list_train,speed_mat_list_test){
#   # creates a speed distance matrix between the test paths and training paths
#   # contains the correct L2 norm structure sum of square distances (obv)
# 
#   mat_train = t(sapply(speed_mat_list_train,function(x) x[[1]]))
#   mat_test  = t(sapply(speed_mat_list_test,function(x) x[[1]]))
# 
#   n_test  = nrow(mat_test)
#   n_train = nrow(mat_train)
#   
#   both = rbind(mat_train,mat_test)
#   all_dist = as.matrix(dist(both))
# 
#   output_mat = all_dist[(n_train+1):(n_test+n_train),1:n_train]
# 
#   return(output_mat)
# }
