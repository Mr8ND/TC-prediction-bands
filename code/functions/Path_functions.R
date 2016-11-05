library(geosphere)
library(datamart)

swap2DfCols = function(data.df){
	return(data.frame(cbind(data.df[,2],data.df[,1])))
}

distAlongP = function(data.df, output_length="nautical mile", longlat=TRUE){
	#This function calculates the distance and bearing between each point of a path.

	#Input is a (n by 2) dataframe in which each row consists of a pair of values.
	#Default is that the pair is (longitude, latitude), which is the flag variable
	#"longlat". If this is set to false then the dataframe is simply swapped.

	#Ouput is a list with two vectors.
	#The first vector is the distance, which is usually outputted in nautical miles.
	#If a different length is need to be used, then the variable "output_length"
	#takes a length measure, according to the length measures used by the package
	#"datamart".
	#The second vector outputs the vector of bearings, according to the usual notation.
	#Please note that given a path of n points, length of output vector would be n-1.

	if (longlat==FALSE){
		print("-- Formatting the df in longitude - latitude form --")
		data.df = swap2DfCols(data.df)
	}

	n = dim(data.df)[1]
	output_d = numeric(n-1)
	output_b = numeric(n-1)

	for (i in c(1:(n-1))){
		output_d[i] = uconv(distGeo(data.df[i,], data.df[i+1,]), "m", output_length, "Length")
		output_b[i] = bearing(data.df[i,], data.df[i+1,])
	}

	return(list(output_d,output_b))
}


distBetweenP = function(data.df.p1, data.df.p2, output_length="nautical mile", longlat=TRUE){
	#This function calculates the distance between each point of every path.

	#Inputs are two (n by 2) dataframe in which each row consists of a pair of values.
	#Default is that the pair is (longitude, latitude), which is the flag variable
	#"longlat". If this is set to false then both the dataframes will be simply swapped.

	#Ouputs is a is a vector of n distances, which is usually outputted in nautical miles.
	#If a different length is need to be used, then the variable "output_length"
	#takes a length measure, according to the length measures used by the package
	#"datamart".

	if (longlat==FALSE){
		print("-- Formatting the dfs in longitude - latitude form --")
		data.df.p1 = swap2DfCols(data.df.p1)
		data.df.p2 = swap2DfCols(data.df.p2)
	}

	n = dim(data.df.p1)[1]
	if (n != dim(data.df.p1)[1]){
		stop("DFs have different dimension")
	}
	output_d = numeric(n-1)

	for (i in c(1:n)){
		output_d[i] = uconv(distGeo(data.df.p1[i,], data.df.p2[i,]), "m", output_length, "Length")
	}

	return(output_d)
}


distMatrixPath = function(path_mat_list, output_length="nautical mile", longlat=TRUE){
	#This function calculates the distance matrix Delta between n paths.
	#This matrix is a symmetric matrix, with the diagonal being 1, and each of the off
	#diagonal element is the sum of the absolute distance of each point of the two paths.

	#Input is a list of path. Each path is a matrix of (n x 2) elements, in which each
	#row is a pair of longitude latitude points.
	#Default is that the pair is (longitude, latitude), which is the flag variable
	#"longlat". If this is set to false then both the dataframes will be simply swapped.

	#Ouput is the delta matrix distance, output as a matrix (n x n).

	n_mat = length(path_mat_list)
	output_mat = matrix(, nrow=n_mat, ncol=n_mat)

	for (i in c(1:n_mat)){
		for (j in c(i:n_mat)){
			output_mat[i,j] = sum(distBetweenP(path_mat_list[[i]], path_mat_list[[j]],longlat=longlat))
			output_mat[j,i] = output_mat[i,j]
		}
	}

	return(output_mat)
}


probMatrixPath = function(dist_matrix, epsilon=430){
	#Given a certain distance Matrix D, this function gives back the probability matrix
	#indicated in the paper, by first trasforming it using the exp(-D/epsilon).
	#Epsilon is by default 430, but it can be changed using the related variable.

	#The input is a matrix (n x n).
	#Ouput is a matrix (n x n), with the rows summing to one.

	w = exp(-1*(dist_matrix/epsilon))
	return(w/rowSums(w))
}


d2MatrixPath = function(prob_matrix, t=5){
	#Given a certain probability matrix P, this function computes the matrix D^2(x,y)
	#defines as follows:
	#   D^2(x,y) = sum_k (P(x,k)-P(y,k))/stat_k
	#Where k is the stationary vector of the probability matrix.

	#NOTE: This function always assumes that the probability matrix is built correctly
	#and that all the rows sums to one, such that an eigenvalue 1 always exists - and 
	#it is the biggest of the series. So the function goes and looks for the first
	#eigenvector of the series.

	#The input is the probability matrix (n x n)

	#The output is the D^2(x,y) matrix (n x n)


	prob_mat_t = prob_matrix**t
	eigenvec_1 = eigen(prob_matrix)$vectors[,1]

	n_mat= dim(prob_mat_t)[1]
	output_mat = matrix(, nrow=n_mat, ncol=n_mat)

	for (i in c(1:n_mat)){
		for (j in c(i:n_mat)){
			d2_vec = (prob_matrix[i,] - prob_matrix[j,])/(eigenvec_1**(1/2))
			output_mat[i,j] = as.numeric(d2_vec %*% d2_vec)
			output_mat[j,i] = output_mat[i,j] 
		}
	}

	return(output_mat)
}

