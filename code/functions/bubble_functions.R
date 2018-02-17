#bubble_steps_CI_np = bubbleCI(dflist, tc.weight.vec, level = alpha.value, direct_sel = TRUE, direct_sel_idx = depth_vector_idx[1])
#NSWE.lists.np = calculateErrorBandsBubble(bubble_steps_CI_np, conversion=TRUE)
#error.NS.np = NSWE.lists.np[[1]]
#error.EW.np = NSWE.lists.np[[2]]
#center.radius.np = NSWE.lists.np[[3]]

#center.radius.df.np = c()
#for (i in c(1:length(center.radius.np))){
#  center.radius.df.np = c(center.radius.df.np, center.radius.np[[i]][c(1,2,3)])
#}
#center.radius.df.np = data.frame(t(matrix(center.radius.df.np, nrow=3)))

#in.vec.np = checkPointsInBands(true.curve,center.radius.df.np)
#result.mat[idx, 3] = sum(in.vec.np)/length(in.vec.np)


#' Library --------------------------------
library(datamart)
library(geosphere)


#' Functions ------------------------------

#' Rearranging list of simulated TCs
#' 
#' @description
#' This function takes as an input the list of the simulated TCs, with a path being a dataframe of (k x 2), 
#' where k is the number of points of the path. The function selects the "center" TC in order to get the
#' length of this path k_0. This functions creates a list of k_0 elements, in which longitude and latitudes
#' are saved in first and second column respectively.
#' 
#' @param dflist List of simulated TCs
#' @param center_idx Index of the path that needs to be considered as a center
#' @param long Column index of the longitude
#' @param lat Column index of the latitude
#' 
#' @return List with same number of elements as the points in the track of the "central" TC. Each element j in
#' the list is a dataframe with the j-th points of the simulated TCs.
#' 
rearrange_dflist_bubble <- function(dflist, center_idx, long = 1, lat = 2) {
  
  n <- length(dflist)
  n_df <- dim(dflist[[center_idx]])[1]
  output_mat <- matrix(, nrow=n_df, ncol=2*n)
  output_list <- list()

  for (j in c(1:n_df)) {

    for (i in c(1:n)) {
      output_mat[j, (2*i - 1)] <- dflist[[i]][j, long]
      output_mat[j, (2*i)] <- dflist[[i]][j, lat]
    }

    output_list[[j]] <- data.frame(t(matrix(output_mat[j, ], ncol = n)))
    output_list[[j]] <- output_list[[j]][!(is.na(output_list[[j]][, 1])), ]
    colnames(output_list[[j]]) = c("long", "lat")
  }
  return(output_list)
}


#' Creates a CI bubble of 1-alpha level with a series of longitude-latitude points
#' 
#' @description
#' This function return the 1-alpha percentage points closest to a point which belongs to
#' the "central" simulated TC.
#' 
#' @param df_points_step_track Dataframe of points - these corresponds to all the points at a certain
#' point of the TCs track. Dataframe should only contain latitude and longitude.
#' @param center_idx Index of the point that needs to be considered as a center
#' @param alpha_level Alpha level to which the bubble needs to be generated
#' @param output_length Unit of measure of the output.
#' 
#' @return Dataframe with the points inside the 1-alpha level, with distance attached to the original dataframe.
#' 
create_CI_bubble_step_track <- function(df_points_step_track, center_idx, alpha_level = .10, 
                                        output_length = "nautical mile"){
  #This function is the engine between a dataframe of points and probabilities and calculating the points in
  # alpha-level bubble.
  
  #The input is a df which should be the collection of all k^th point of n paths with the associated probability of the 
  #path. In other words, we have n pair of points (longitude, latitude) and the probability associated to the path
  #at each row of the dataframe.
  #The level for the CI is supposed to be 95, but it can be tuned.
  
  #The output is a single dataframe, in which the closest points around the point of the maximum likely path  
  #such that the sum of their probability is above the level requested are output.
  
  level <- 1 - alpha_level
  n_df <- dim(df_points_step_track)[1]

  # Calculating distance
  distance_vec <- numeric(n_df)
  for (j in c(1:n_df)) {
    distance_vec[j] <- uconv(distGeo(df_points_step_track[center_idx, ], df_points_step_track[j, ]), "m", output_length, "Length")
  }
  
  # In order to select the level, we select the closest (1-alpha)% of them
  ordered_path_prob_df <- data.frame(cbind(df_points_step_track, distance_vec))
  ordered_path_prob_df <- ordered_path_prob_df[order(ordered_path_prob_df[, 3), ]
  ind_sel <- ceiling(n_df * (1-alpha_level))
  out_df <- ordered_path_prob_df[c(1:ind_sel), ]
  
  return(out_df)
}


#' Estimating the CI around a given path and a given level with symmetrical radius
#' 
#' @description 
#' This function is a wrapper around rearrange_dflist_bubble and create_CI_bubble_step_track.
#' First rearranges the dflist of simulated TC and then calculates the 1-alpha level bubble for
#' each of the track points of the "central" TC.
#'
#' @param dflist List of simulated TCs
#' @param center_idx Index of the path that needs to be considered as a center
#' @param long Column index of the longitude
#' @param lat Column index of the latitude
#' @param output_length Unit of measure of the output
#
#' @return Matrix with prediction and, if alpha_level was not NULL, extra column on
#' whether the point is within a specific 1-alpha_level countour.
#'
bubbleCI <- function(dflist, center_idx, alpha_level = 0.1, long = 1, lat = 2, output_length = "nautical mile"){
  
  points_list <- rearrange_dflist_bubble(dflist = dflist, center_idx = center_idx, 
                                        long = long, lat = lat)
  
  n = length(points_list)
  output_list = list()
  for (j in c(1:n)) { 
    output_list[[j]] <- create_CI_bubble_step_track(df_points_step_track = points_list[[j]], 
                                                    center_idx = center_idx, 
                                                    alpha_level = alpha_level, 
                                                    output_length = output_length)
  }
  
  return(output_list)
}


#' Estimating uncertainty around Bubble CIs
#' 
#' @description
#' This function takes in a dataframe of points, which are representing the points inside a bubble
#' CI and calculates the length of the radius in both north-south direction (NS) and east-west (EW)
#' direction, along with the center of each of these.
#' 
#' @param bubble_ci_list list of dataframe, in which each represent a bubble CI around a point of the 
#' "central" TC
#' @param long Column index of the longitude
#' @param lat Column index of the latitude
#' @param distance Column index of the distance column between points
#' @param conversion_unit_distance Optional parameter to include unit measure conversion on distances.
#' Distances are assumed to be passed in as "nautical miles".
#' 
#' @return A list of 4 elements. The first two are the lists of extreme points in the NS and EW
#' directions, the third one is a list the central point with the maximum distance of the CI bubble and
#' the fourth one is the third lists reduced to a dataframe.
#' 
error_bands_bubbleCI <- function(bubble_ci_list, long = 1, lat = 2, distance = 3, 
                                     conversion_unit_distance = 'm') {

  error_bands_NS <- list()
  error_bands_EW <- list()
  center_radius_list <- list()
  n <- length(bubble_ci_list)

  for (i in c(1:n)){

    # Calculating center point
    bubble.df.cons <- bubble.df[[i]][order(bubble.df[[i]][, distance], decreasing = TRUE), ]
    center_point <- as.numeric(bubble.df.cons[1, c(long, lat)])

    # Calculating max distance from the center point
    max_distance <- max(bubble.df.cons[,distance])
    if (!is.null(conversion_unit_distance)) {
      max_distance <- uconv(max_distance, "nautical mile", "m", "Length")
    }

    # Calculating extremes of the bubble CI in all directions
    northern.point <- destPoint(center_point, 0, max_distance)
    southern.point <- destPoint(center_point, -180, max_distance)
    eastern.point <- destPoint(center_point, 90, max_distance)
    western.point <- destPoint(center_point, -90, max_distance)
    
    error_bands_NS[[i]] <- c(northern.point, southern.point)
    error_bands_EW[[i]] <- c(eastern.point, western.point)
    center_radius_list[[i]] <- c(center_point, max_distance)
  }

  # Reducing the center_radius_list to a df as well (necessary later)
  center_radius_df = c()
  for (i in c(1:length(center_radius_list))) {
    center_radius_df = c(center_radius_df, center_radius_list[[i]][c(1,2,3)])
  }
  center_radius_df = data.frame(t(matrix(center_radius_df, nrow=3))
  
  return(list('errors_NS' = error_bands_NS, 'errors_EW' = error_bands_EW,
               'center_points_list_distance' = center_radius_list,
               'center_points_df_distance' = center_radius_df))
}


#' Check if points are inside CI bubble
#' 
#' @description
#' Given a dataframe of points this function returns a vector of the same
#' length with 1 or 0 whether the respective points is included in any of the CI
#' bubble.
#' 
#' @param df_points Dataframe of points which inclusion needs to be calculated
#' @param center_radius_df Dataframe of central points with associated radius of the CI
#' bubble
#' @param long column index of the longitude in the central points
#' @param lat column index of the latitude in the central points
#' @param radius column index of the radius in the central points
#' 
#' @output Boolean vectors on whether the points are in any of the bubble CIs or not
check_points_in_bubbleCI <- function(df_points, center_radius_df, long = 1, lat = 2, radius =3){

  n_df <- dim(df_points)[1]
  in_vec <- numeric(n_df)
  
  for (i in c(1:n_df)) {

    for (c in c(1:dim(center_radius_df)[1])) {
      dist.measured <- distGeo(as.numeric(center_radius_df[c, c(1,2)]), as.numeric(df_points[i, c(1,2)]))

      if (abs(dist.measured) <= abs(center_radius_df[c, 3]) || 
        ((df_points[i, 1] == center_radius_df[c, 1]) &&  (df_points[i, 2] == center_radius_df[c, 2])) ) {
        in_vec[i] <- 1
      }
    }
  }  
  return(in_vec)
}