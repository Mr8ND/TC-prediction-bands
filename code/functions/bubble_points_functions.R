library(datamart)
library(geosphere)

rearrangePathList = function(list_path, prob_vec_path,long=1,lat=2,bear=3,bear.change=4,distance=6){
	#This function takes as an input the list of the path, in which we have n elements, each of with is a path.
	#A path is a dataframe of (k x 2), where k is the number of points of the path. k needs to be the same for 
	#every path in the list or it will throw an error. the number of columns needs to be 2 as well or it is going
	#to throw an error as well.
	#The prob_vec_path is the probability or likelihood associated to each path. The probability vector is an n
	#dimensional vector.

	#This function goes through all the path and creates a matrix with k rows and 2n columns.
	#For each of the k points, it will store the longitude and latitude of the first path in the first and second position
	# and so on for all the n path.

	#The output is a list of length k in which each element are all the k^th points of all the n path, with associated
	#the probability of such path. Every element of the output is a dataframe.

	n = length(list_path)
	n_df = dim(list_path[[1]])[1]
	if (n_df != 13){
		print("Number of points in each dataset was should have 13 but it is not - just flagging it.")
	}
  
	output_mat = matrix(, nrow=n_df, ncol=5*n)
	output_list = list()
	for (j in c(1:n_df)){
		for (i in c(1:n)){
			output_mat[j,(5*i-4)] = list_path[[i]][j,long]
			output_mat[j,(5*i-3)] =list_path[[i]][j,lat]
			output_mat[j,(5*i-2)] =list_path[[i]][j,bear]
			output_mat[j,(5*i-1)] =list_path[[i]][j,bear.change]
			output_mat[j,(5*i)] =list_path[[i]][j,distance]
		}
		output_list[[j]] = data.frame(cbind(t(matrix(output_mat[j,], ncol=n)),prob_vec_path))
		colnames(output_list[[j]]) = c("long", "lat", "bearing", "bearing.change", "distance", "probability")
	}
	return(output_list)
}


selectCIPath = function(path_prob_df, level=.95, output_length="nautical mile"){
	#This function is the engine between a dataframe of points and probabilities and calculating the points in
	# alpha-level bubble.

	#The input is a df which should be the collection of all k^th point of n paths with the associated probability of the 
	#path. In other words, we have n pair of points (longitude, latitude) and the probability associated to the path
	#at each row of the dataframe.
	#The level for the CI is supposed to be 95, but it can be tuned.

	#The output is a single dataframe, in which the closest points around the point of the maximum likely path  
	#such that the sum of their probability is above the level requested are output.

	ind_max_prob = which(path_prob_df[,3]==max(path_prob_df[,3]))
	if (length(ind_max_prob) > 1){
		print('There is a case of tie in calculating the most likely path. Taking the first one as default')
		ind_max_prob = ind_max_prob[1]
	}

	n_df = dim(path_prob_df)[1]
	distance_vec = numeric(n_df)

	for (j in c(1:n_df)){
		distance_vec[j] = uconv(distGeo(path_prob_df[ind_max_prob,], path_prob_df[j,]), "m", output_length, "Length")
	}

	ordered_path_prob_df = data.frame(cbind(path_prob_df, distance_vec))
	ordered_path_prob_df = ordered_path_prob_df[order(ordered_path_prob_df[,4]),]

	ord_prob = ordered_path_prob_df[,3]
	ind_sel = which(cumsum(ord_prob) > level)[1]

	return(ordered_path_prob_df[c(1:ind_sel),])
}


bubbleCI = function(list_path, prob_vec_path, level=.95){

	#This function is simply a wrapper around rearrangePathList and selectCIPath.

	#INPUT: list_path, a list of n path with k points and prob_vec_path, a vector of n element corresponding to the
	#probability of each path.

	#OUPUT: A list with k dataframes, each of which has the closest points around the point of the maximum likely path  
	#such that the sum of their probability is above the level requested.

	points_prob_list = rearrangePathList(list_path, prob_vec_path)

	n = length(points_prob_list)
	output_list = list()
	for (j in c(1:n)){
		output_list[[j]] = selectCIPath(points_prob_list[[j]], level=level)
	}

	return(output_list)
}