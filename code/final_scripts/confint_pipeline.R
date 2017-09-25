#############
# Libraries #
#############

library(datamart)
library(geosphere)
library(datamart)
library(geosphere)
library(plyr)
library(rworldmap)
library(caret)
library(ks)
require(gtools)

#############
# Locations #
#############

project_location      = ""
functions_loc         = "code/functions/"
weights_loc           = "data/"
true_curve_loc        = "data/training/validate/"
sim_curve_loc         = "data/"

findFilesInFolder = function(subfolder, name.pattern, path.out=FALSE){
  temp = list.files(path=subfolder, pattern=name.pattern)
  if (path.out==TRUE){
    for (i in c(1:length(temp))){
      temp[i] = paste0(subfolder,temp[i])
    }
  }
  return(temp)
}

weight.files = mixedsort(findFilesInFolder(weights_loc, "estimate_p", path.out=TRUE))
sim.curve.folders = findFilesInFolder(sim_curve_loc, "Val_Sims", path.out=TRUE)
tc.code.list = findFilesInFolder(sim.curve.folders[1], "AL")

# Limit the tc.code.list to the first 100
limit.tc = 114
tc.code.list = tc.code.list[c(1:limit.tc)]

# True Curve file list
true.curve.file.vec = rep_len("", length.out=limit.tc)
for (j in c(1:limit.tc)){
  true.curve.file.vec[j] = paste0(true_curve_loc, findFilesInFolder(true_curve_loc, tc.code.list[j]))
}

# Creating a list with all the location files according to the sim.curve.folders.
# Every list is going to have a list in which to look for the simulated TC of the TC with that
# specific code.

sim.curve.folders.list = list()
for (j in c(1:4)){
  path.vec = rep_len("", length.out=length(tc.code.list))
  for (i in c(1:length(tc.code.list))){
    path.vec[i] = findFilesInFolder(paste0(sim.curve.folders[j],'/'), tc.code.list[i] , path.out=TRUE)
    path.vec[i] = paste0(path.vec[i], '/')
  }
  sim.curve.folders.list[[j]] = path.vec
}
names(sim.curve.folders.list) = c('auto_d', 'auto_nd', 'no_auto_d', 'no_auto_nd')

#####################
# Loading functions #
#####################

# KDE Functions

flattenTCListWeight = function(dflist, weight.vec){
  # Dflist in this case is a list with n different hurricanes. Usually we have n=100.
  # Each element of such list a dataframe with at least longitude and latitude.
  # This function append by row all dataframes in the list and adds a column, which has the same
  # value for each TC, indicating the weight assigned to such TC.
  # The output is a dataframe will all the dataframes appended on the list and the a column
  # with the weights of the corresponding TC appended.
  
  # Initializing the first step of the process, by creating a vector with the weight of the
  # first TC and appending it to the dataframe in the first position of the list.
  weight.spec.mat = rep(weight.vec[1], nrow(dflist[[1]]))
  dfmat = cbind(dflist[[1]], weight.spec.mat)
  
  # Iterating the same process for all the dataframe in the list
  for (i in c(2:length(dflist))){
    weight.spec.mat = rep(weight.vec[i], nrow(dflist[[i]]))
    dfmat = rbind(dfmat, cbind(dflist[[i]], weight.spec.mat))
  }
  
  return(dfmat)
}

fitKDEObject = function(dfmat, h.band=NULL, long=1, lat=2, weight=3, grid.size=1000){
  
  # This function simply the KDE object given a matrix which has the dataframes TC for training
  # appended to it. By default it considers longitude as first column, latitude as second and
  # weight as third. The default grid size for fitting the KDE is 1000 - it can be reduced
  # to speed up the computation times.
  
  if (!is.null(h.band)){
    h.mat = diag(2)*h.band
    kde.obj = kde(dfmat[,c(long,lat)], w=dfmat[,weight], gridsize=c(grid.size), H=h.mat)
  } else {
    kde.obj = kde(dfmat[,c(long,lat)], w=dfmat[,weight], gridsize=c(grid.size))
  }
  return(kde.obj)
}

predictKDEObject = function(kde.obj, predict.mat, alpha.level=NULL, long=1, lat=2){
  
  # This function, given a KDE object and a matrix to be predicted - ideally a test matrix
  # predicts the density value for the values that needed to be predicted.
  # The values are appended to the matrix and returned.
  # If an alpha.level is entered, then an extra column will be added, in which 1 means that
  # the value is above the alpha.level contour - i.e. within that probability contour - else 0
  # is returned.
  
  # The prediction mat is formatted and the prediction is performed
  predict.mat.kdefit = predict.mat[,c(long,lat)]
  predict.vec = predict(kde.obj, x = predict.mat.kdefit, zero.flag = TRUE)
  out.mat = cbind(predict.mat, predict.vec)
  
  # If the alpha level is selected, then the function will select the right level from the
  # kde.obj$cont vector and store it for comparison.
  if (!is.null(alpha.level)){
    contour.alpha.level = as.numeric(kde.obj$cont[paste(as.character((1-alpha.level)*100), "%", sep="")])
    in.alpha.vec = as.numeric(predict.vec>=contour.alpha.level)
    out.mat = cbind(out.mat, in.alpha.vec)
  }
  
  return(out.mat)
}


# Bubble Points Functions

rearrangePathList = function(list_path, prob_vec_path, max_n = FALSE, long=1, lat=2, direct_selection=TRUE, direct_selection_idx=NULL){
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
  #n_df = dim(list_path[[1]])[1]
  #if (n_df != 13){
  #  print("Number of points in each dataset was should have 13 but it is not - just flagging it.")
  #}
  
  if (max_n == TRUE){
    n_df.vec = numeric(n)
    for (j in c(1:n)){
      n_df.vec[j] = dim(list_path[[j]])[1]
    }
    n_df = max(n_df.vec)
  }
  else{
    ind_max_prob = which(prob_vec_path==max(prob_vec_path))[1]
    n_df = dim(list_path[[ind_max_prob]])[1]
  }
  
  if (direct_selection == TRUE){
    ind_max_prob = direct_selection_idx
    n_df = dim(list_path[[ind_max_prob]])[1]
  }
  
  output_mat = matrix(, nrow=n_df, ncol=2*n)
  output_list = list()
  for (j in c(1:n_df)){
    for (i in c(1:n)){
      output_mat[j,(2*i-1)] = list_path[[i]][j,long]
      output_mat[j,(2*i)] =list_path[[i]][j,lat]
    }
    output_list[[j]] = data.frame(cbind(t(matrix(output_mat[j,], ncol=n)),prob_vec_path))
    output_list[[j]] = output_list[[j]][!(is.na(output_list[[j]][,1])),]
    colnames(output_list[[j]]) = c("long", "lat","probability")
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
  if (is.na(ind_sel)){
    ind_sel = length(ord_prob)
  }
  
  return(ordered_path_prob_df[c(1:ind_sel),])
}

bubbleCI = function(list_path, prob_vec_path, level=.95, max_n=FALSE, direct_sel=FALSE, direct_sel_idx=NULL){
  
  #This function is simply a wrapper around rearrangePathList and selectCIPath.
  
  #INPUT: list_path, a list of n path with k points and prob_vec_path, a vector of n element corresponding to the
  #probability of each path.
  
  #OUPUT: A list with k dataframes, each of which has the closest points around the point of the maximum likely path  
  #such that the sum of their probability is above the level requested.
  
  points_prob_list = rearrangePathList(list_path, prob_vec_path, max_n=max_n,
                                       direct_selection=direct_sel, direct_selection_idx=direct_sel_idx)
  
  n = length(points_prob_list)
  output_list = list()
  for (j in c(1:n)){
    output_list[[j]] = selectCIPath(points_prob_list[[j]], level=level)
  }
  
  return(output_list)
}

calculateErrorBandsBubble = function(bubble.df, long=1, lat=2, distance=4, conversion=TRUE){
  error_bands_NS = list()
  error_bands_EW = list()
  center_radius_list = list()
  
  n = length(bubble.df)
  for (i in c(1:n)){
    bubble.df.cons = bubble.df[[i]][order(bubble.df[[i]][,3],decreasing = TRUE),]
    max_likelihood_point = as.numeric(bubble.df.cons[1,c(long, lat)])
    max_distance = max(bubble.df.cons[,distance])
    
    if (conversion==TRUE){
      dist.meters = uconv(max_distance, "nautical mile", "m", "Length")}
    else{ dist.meters = max_distance}
    northern.point = destPoint(max_likelihood_point, 0, dist.meters)
    southern.point = destPoint(max_likelihood_point, -180, dist.meters)
    eastern.point = destPoint(max_likelihood_point, 90, dist.meters)
    western.point = destPoint(max_likelihood_point, -90, dist.meters)
    
    error_bands_NS[[i]] = c(northern.point, southern.point)
    error_bands_EW[[i]] = c(eastern.point, western.point)
    center_radius_list[[i]] = c(max_likelihood_point, dist.meters)
  }
  
  return(list(error_bands_NS, error_bands_EW,center_radius_list))
}

checkPointsInBands = function(df, center.radius.df){
  n.df = dim(df)[1]
  in.vec = numeric(n.df)
  
  for (i in c(1:n.df)){
    for (c in c(1:dim(center.radius.df)[1])){
      dist.measured = distGeo(as.numeric(center.radius.df[c,c(1,2)]),
                              as.numeric(df[i,c(1,2)]))
      if (abs(dist.measured) <= abs(center.radius.df[c,3])||
          ((df[i,1]==center.radius.df[c,1]) && 
           (df[i,2]==center.radius.df[c,2]))){
        in.vec[i] = 1
      }
    }
  }  
  return(in.vec)
}


# Non-parametric estimation of the curves

source(paste0(functions_loc, '13pointreduction.R'))
source(paste0(functions_loc, 'Path_functions.R'))
source('code/functions/depth_function.R')


############
# Pipeline #
############

confintPipeline = function(sim.curve.folders.list, true.curve.file.vec, weight.files, curve.type,
                           long.true.curves = 6, lat.true.curves = 5, max_n_bubble=FALSE,
                           alpha.value = .9){
  
  # curve.type can be one out of 'auto_d', 'auto_nd', 'no_auto_d' or 'no_auto_nd'
  if (!is.element(curve.type, c('auto_d', 'auto_nd', 'no_auto_d', 'no_auto_nd'))){
    stop("curve.type must be one out of 'auto_d', 'auto_nd', 'no_auto_d' or 'no_auto_nd'")
  }
  # First we select the folders from which to take the simulation curves
  sim.curve.folders = sim.curve.folders.list[[curve.type]]
  
  # Initialize the result matrix
  result.mat = matrix(, nrow=length(sim.curve.folders), ncol=3)
  
  for (idx in c(1:length(sim.curve.folders))){
    
    # Loading in the simulated curve
    temp = findFilesInFolder(sim.curve.folders[idx], "_sim_", path.out=TRUE)
    dflist = lapply(temp, function(x) data.frame(read.table(x, header=TRUE, sep=",")))
    
    # Loading in the true curve. We automatic assume that longitude is in the 6th curve for true
    # curves and latitude is in 5
    true.curve = data.frame(read.table(true.curve.file.vec[idx], header=FALSE, sep=" "))[,c(long.true.curves,lat.true.curves)]
    names(true.curve) = c("long", "lat")
    
    # Loading in the right weight and normalizing them
    idx.weights = (idx-1) %/% 25
    load(weight.files[idx.weights+1])
    tc.weight.vec = estimate_p[[curve.type]][[idx]]$p_estimate_test
    tc.weight.vec = tc.weight.vec/sum(tc.weight.vec)
    
    # KDE APPROACH
    dfmat = flattenTCListWeight(dflist, tc.weight.vec)
    kde.obj = fitKDEObject(dfmat)
    predicted.kde.mat = predictKDEObject(kde.obj, true.curve, alpha.level = alpha.value)
    result.mat[idx, 1] = sum(predicted.kde.mat[,4])/nrow(predicted.kde.mat)
    
    #BUBBLE APPROACH
    bubble_steps_CI = bubbleCI(dflist, tc.weight.vec, max_n=max_n_bubble, level = alpha.value)
    NSWE.lists = calculateErrorBandsBubble(bubble_steps_CI, conversion=TRUE)
    error.NS = NSWE.lists[[1]]
    error.EW = NSWE.lists[[2]]
    center.radius = NSWE.lists[[3]]
    
    center.radius.df = c()
    for (i in c(1:length(center.radius))){
      center.radius.df = c(center.radius.df, center.radius[[i]][c(1,2,3)])
    }
    center.radius.df = data.frame(t(matrix(center.radius.df, nrow=3)))
    
    in.vec = checkPointsInBands(true.curve,center.radius.df)
    result.mat[idx, 2] = sum(in.vec)/length(in.vec)
    
    
    #BUBBLE APPROACH WITH NONPARAMETRIC
    dflist_13pointsreduction = thirteen_points_listable(dflist, c_position = 1:2)
    dist_matrix_13pointsreduction = distMatrixPath(dflist_13pointsreduction)
    depth_vector = depth_function(dist_matrix_13pointsreduction)
    depth_vector_idx = which(depth_vector==max(depth_vector))
    
    bubble_steps_CI_np = bubbleCI(dflist, tc.weight.vec, level = alpha.value, direct_sel = TRUE, direct_sel_idx = depth_vector_idx[1])
    NSWE.lists.np = calculateErrorBandsBubble(bubble_steps_CI_np, conversion=TRUE)
    error.NS.np = NSWE.lists.np[[1]]
    error.EW.np = NSWE.lists.np[[2]]
    center.radius.np = NSWE.lists.np[[3]]
    
    center.radius.df.np = c()
    for (i in c(1:length(center.radius.np))){
      center.radius.df.np = c(center.radius.df.np, center.radius.np[[i]][c(1,2,3)])
    }
    center.radius.df.np = data.frame(t(matrix(center.radius.df.np, nrow=3)))
    
    in.vec.np = checkPointsInBands(true.curve,center.radius.df.np)
    result.mat[idx, 3] = sum(in.vec.np)/length(in.vec.np)
  
    
    print(paste("TC number",idx,"done - ", curve.type, "type of curves."))
  }
  return(result.mat)
}


ptm <- proc.time()
example.result.mat = confintPipeline(sim.curve.folders.list=sim.curve.folders.list,
                                     true.curve.file.vec=true.curve.file.vec, 
                                     weight.files=weight.files, 
                                     curve.type='auto_d')
proc.time() - ptm


confintPipelineWrapper = function(sim.curve.folders.list, true.curve.file.vec, weight.files,
                       long.true.curves = 6, lat.true.curves = 5, max_n_bubble=FALSE,
                       alpha.value = .9){
  
  curve.type.vec = c('auto_d', 'auto_nd', 'no_auto_d', 'no_auto_nd')
  result.list = list()
  
  for (j in c(1:length(curve.type.vec))){
    result.list[[j]] = confintPipeline(sim.curve.folders.list = sim.curve.folders.list,
                                 true.curve.file.vec = true.curve.file.vec, 
                                 weight.files = weight.files, 
                                 curve.type = curve.type.vec[j],
                                 long.true.curves = long.true.curves,
                                 lat.true.curves = lat.true.curves,
                                 max_n_bubble = max_n_bubble,
                                 alpha.value = alpha.value)
  }
  names(result.list) = curve.type.vec
  return(result.list)
}


result.list = confintPipelineWrapper(sim.curve.folders.list=sim.curve.folders.list,
                                      true.curve.file.vec=true.curve.file.vec, 
                                      weight.files=weight.files)

save(result.list, file = "confint_result_list_bubbleKDEnonParam.Rdata")


############################
# Pipeline Without Weights #
############################

confintPipelineNoWeights =  function(sim.curve.folders.list, true.curve.file.vec, curve.type,
                                     long.true.curves = 6, lat.true.curves = 5, max_n_bubble=FALSE,
                                     alpha.value = .9){
  
  # curve.type can be one out of 'auto_d', 'auto_nd', 'no_auto_d' or 'no_auto_nd'
  if (!is.element(curve.type, c('auto_d', 'auto_nd', 'no_auto_d', 'no_auto_nd'))){
    stop("curve.type must be one out of 'auto_d', 'auto_nd', 'no_auto_d' or 'no_auto_nd'")
  }
  # First we select the folders from which to take the simulation curves
  sim.curve.folders = sim.curve.folders.list[[curve.type]]
  
  # Initialize the result matrix
  result.mat = matrix(, nrow=length(sim.curve.folders), ncol=2)
  
  for (idx in c(1:length(sim.curve.folders))){
    
    # Loading in the simulated curve
    temp = findFilesInFolder(sim.curve.folders[idx], "_sim_", path.out=TRUE)
    dflist = lapply(temp, function(x) data.frame(read.table(x, header=TRUE, sep=",")))
    
    # Loading in the true curve. We automatic assume that longitude is in the 6th curve for true
    # curves and latitude is in 5
    true.curve = data.frame(read.table(true.curve.file.vec[idx], header=FALSE, sep=" "))[,c(long.true.curves,lat.true.curves)]
    names(true.curve) = c("long", "lat")
    
    #Setting the weights to be uniform
    tc.weight.vec = rep(1, length(dflist))
    
    # KDE APPROACH
    dfmat = flattenTCListWeight(dflist, tc.weight.vec)
    kde.obj = fitKDEObject(dfmat)
    predicted.kde.mat = predictKDEObject(kde.obj, true.curve, alpha.level = alpha.value)
    result.mat[idx, 1] = sum(predicted.kde.mat[,4])/nrow(predicted.kde.mat)
    
    
    #BUBBLE APPROACH WITH NONPARAMETRIC
    dflist_13pointsreduction = thirteen_points_listable(dflist, c_position = 1:2)
    dist_matrix_13pointsreduction = distMatrixPath(dflist_13pointsreduction)
    depth_vector = depth_function(dist_matrix_13pointsreduction)
    depth_vector_idx = which(depth_vector==max(depth_vector))
    
    bubble_steps_CI_np = bubbleCI(dflist, tc.weight.vec, level = alpha.value, direct_sel = TRUE, direct_sel_idx = depth_vector_idx[1])
    NSWE.lists.np = calculateErrorBandsBubble(bubble_steps_CI_np, conversion=TRUE)
    error.NS.np = NSWE.lists.np[[1]]
    error.EW.np = NSWE.lists.np[[2]]
    center.radius.np = NSWE.lists.np[[3]]
    
    center.radius.df.np = c()
    for (i in c(1:length(center.radius.np))){
      center.radius.df.np = c(center.radius.df.np, center.radius.np[[i]][c(1,2,3)])
    }
    center.radius.df.np = data.frame(t(matrix(center.radius.df.np, nrow=3)))
    
    in.vec.np = checkPointsInBands(true.curve,center.radius.df.np)
    result.mat[idx, 2] = sum(in.vec.np)/length(in.vec.np)
    
    
    print(paste("TC number",idx,"done - ", curve.type, "type of curves."))
  }
  return(result.mat)
}


confintPipelineWrapperNoWeights = function(sim.curve.folders.list, true.curve.file.vec,
                                  long.true.curves = 6, lat.true.curves = 5, max_n_bubble=FALSE,
                                  alpha.value = .9){
  
  curve.type.vec = c('auto_d', 'auto_nd', 'no_auto_d', 'no_auto_nd')
  result.list = list()
  
  for (j in c(1:length(curve.type.vec))){
    result.list[[j]] = confintPipelineNoWeights(sim.curve.folders.list = sim.curve.folders.list,
                                       true.curve.file.vec = true.curve.file.vec,
                                       curve.type = curve.type.vec[j],
                                       long.true.curves = long.true.curves,
                                       lat.true.curves = lat.true.curves,
                                       max_n_bubble = max_n_bubble,
                                       alpha.value = alpha.value)
  }
  names(result.list) = curve.type.vec
  return(result.list)
}


result.list.noweights = confintPipelineWrapperNoWeights(sim.curve.folders.list=sim.curve.folders.list,
                                     true.curve.file.vec=true.curve.file.vec)

save(result.list.noweights, file = "confint_result_list_noweights_KDEnonParam.Rdata")