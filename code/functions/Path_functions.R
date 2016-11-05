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