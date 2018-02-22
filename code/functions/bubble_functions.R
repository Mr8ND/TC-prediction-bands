#' Library --------------------------------
library(datamart)
library(geosphere)
library(sp)
library(tidyverse)
library(rgeos)


#' Functions ------------------------------

#' Rearranging list of simulated TCs
#' 
#' @description
#' This function takes as an input the list of the simulated TCs, with a path 
#' being a dataframe of (k x 2), where k is the number of points of the path. 
#' The function selects the "center" TC in order to get the length of this path 
#' k_0. This functions creates a list of k_0 elements, in which longitude and 
#' latitudes are saved in first and second column respectively.
#' 
#' @param dflist List of simulated TCs
#' @param center_idx Index of the path that needs to be considered as a center
#' @param long Column index of the longitude
#' @param lat Column index of the latitude
#' 
#' @return List with same number of elements as the points in the track of the 
#' "central" TC. Each element j in the list is a dataframe with the j-th points 
#' of the simulated TCs.
#' 
rearrange_dflist_bubble <- function(dflist, center_idx, long = 1, lat = 2) {
  
  n <- length(dflist)
  n_df <- dim(dflist[[center_idx]])[1]
  output_mat <- matrix(NA, nrow = n_df, ncol = 2*n)
  output_list <- list()

  for (j in c(1:n_df)) {

    for (i in c(1:n)) {
      output_mat[j, (2*i - 1)] <- ifelse(dim(dflist[[i]])[1] >= j, 
                                         dflist[[i]][j, long], NA)
      output_mat[j, (2*i)] <- ifelse(dim(dflist[[i]])[1] >= j, 
                                     dflist[[i]][j, lat], NA)
    }

    output_list[[j]] <- data.frame(t(matrix(output_mat[j, ], ncol = n)))
    output_list[[j]] <- output_list[[j]][!(is.na(output_list[[j]][, 1])), ]
    colnames(output_list[[j]]) = c("long", "lat")
  }
  return(output_list)
}


#' Creates a CI bubble of 1-alpha level with a series of longitude-latitude 
#' points
#' 
#' @description
#' This function return the 1-alpha percentage points closest to a point which 
#' belongs to the "central" simulated TC.
#' 
#' @param df_points_step_track Dataframe of points - these corresponds to all 
#' the points at a certain point of the TCs track. Dataframe should only contain
#' latitude and longitude.
#' @param center_idx Index of the point that needs to be considered as a center
#' @param alpha_level Alpha level to which the bubble needs to be generated
#' @param output_length Unit of measure of the output.
#' 
#' @return Dataframe with the points inside the 1-alpha level, with distance 
#' attached to the original dataframe.
#' 
create_CI_bubble_step_track <- function(df_points_step_track, center_idx, 
                                        alpha_level = .10, 
                                        output_length = "nautical mile"){
  level <- 1 - alpha_level
  n_df <- dim(df_points_step_track)[1]

  # Calculating distance
  distance_vec <- numeric(n_df)
  for (j in 1:n_df) {
    temp_center_idx <- which(row.names(df_points_step_track) == center_idx)
    distance_vec[j] <- datamart::uconv(
                            geosphere::distGeo(
                              df_points_step_track[temp_center_idx, ],
                              df_points_step_track[j, ]), 
                            "m", output_length, "Length")
  }
  
  # In order to select the level, we select the closest (1-alpha)% of them
  ordered_path_prob_df <- data.frame(cbind(df_points_step_track, distance_vec))
  ordered_path_prob_df <- ordered_path_prob_df[
                                        order(ordered_path_prob_df[, 3]), ]
  ind_sel <- ceiling(n_df * (1 - alpha_level))
  out_df <- ordered_path_prob_df[c(1:ind_sel), ]
  
  return(out_df)
}


#' Estimating the CI around a given path and a given level with symmetrical 
#' radius
#' 
#' @description 
#' This function is a wrapper around rearrange_dflist_bubble and 
#' create_CI_bubble_step_track. First rearranges the dflist of simulated TC and 
#' then calculates the 1-alpha level bubble for each of the track points of the 
#' "central" TC.
#'
#' @param dflist List of simulated TCs
#' @param center_idx Index of the path that needs to be considered as a center
#' @param long Column index of the longitude
#' @param lat Column index of the latitude
#' @param output_length Unit of measure of the output
#
#' @return Matrix with prediction and, if alpha_level was not NULL, extra column
#' on whether the point is within a specific 1-alpha_level countour.
#'
bubbleCI <- function(dflist, center_idx, alpha_level = 0.1, long = 1, lat = 2, 
                     output_length = "nautical mile"){
  
  points_list <- rearrange_dflist_bubble(dflist = dflist, 
                                         center_idx = center_idx,
                                         long = long, lat = lat)
  
  n <- length(points_list)
  output_list = list()
  for (j in 1:n) { 
    output_list[[j]] <- create_CI_bubble_step_track(
                              df_points_step_track = points_list[[j]], 
                              center_idx = center_idx, 
                              alpha_level = alpha_level, 
                              output_length = output_length)
  }
  
  return(output_list)
}

#' 
#' #' Estimating uncertainty around Bubble CIs - NSWE Direction Approach
#' #' 
#' #' @description
#' #' This function takes in a dataframe of points, which are representing the 
#' #' points inside a bubble CI and calculates the length of the radius in both 
#' #' north-south direction (NS) and east-west (EW) direction, along with the 
#' #' center of each of these.
#' #' 
#' #' @param bubble_ci_list list of dataframe, in which each represent a bubble CI 
#' #' around a point of the "central" TC
#' #' @param long Column index of the longitude
#' #' @param lat Column index of the latitude
#' #' @param distance Column index of the distance column between points
#' #' @param conversion_unit_distance Optional parameter to include unit measure 
#' #' conversion on distances. Distances are assumed to be passed in as 
#' #' "nautical miles".
#' #' 
#' #' @return A list of 4 elements. The first two are the lists of extreme points 
#' #' in the NS and EW directions, the third one is a list the central point with 
#' #' the maximum distance of the CI bubble and the fourth one is the third lists 
#' #' reduced to a dataframe.
#' #' 
#' error_bands_bubbleCI_NSWE <- function(bubble_ci_list, long = 1, lat = 2, 
#'                                       distance = 3, 
#'                                       conversion_unit_distance = 'm') {
#' 
#'   error_bands_NS <- list()
#'   error_bands_EW <- list()
#'   center_radius_list <- list()
#'   n <- length(bubble_ci_list)
#' 
#'   for (i in 1:n) {
#' 
#'     # Calculating center point
#'     bubble.df.cons <- bubble.df[[i]][order(bubble.df[[i]][, distance],
#'                                            decreasing = TRUE), ]
#'     center_point <- as.numeric(bubble.df.cons[1, c(long, lat)])
#' 
#'     # Calculating max distance from the center point
#'     max_distance <- max(bubble.df.cons[,distance])
#'     if (!is.null(conversion_unit_distance)) {
#'       max_distance <- datamart::uconv(max_distance, "nautical mile",
#'                                       "m", "Length")
#'     }
#' 
#'     # Calculating extremes of the bubble CI in all directions
#'     northern.point <- geosphere::destPoint(center_point, 0, max_distance)
#'     southern.point <- geosphere::destPoint(center_point, -180, max_distance)
#'     eastern.point <- geosphere::destPoint(center_point, 90, max_distance)
#'     western.point <- geosphere::destPoint(center_point, -90, max_distance)
#'     
#'     error_bands_NS[[i]] <- c(northern.point, southern.point)
#'     error_bands_EW[[i]] <- c(eastern.point, western.point)
#'     center_radius_list[[i]] <- c(center_point, max_distance)
#'   }
#' 
#'   # Reducing the center_radius_list to a df as well (necessary later)
#'   center_radius_df = c()
#'   for (i in c(1:length(center_radius_list))) {
#'     center_radius_df <- c(center_radius_df, center_radius_list[[i]][c(1,2,3)])
#'   }
#'   center_radius_df <- data.frame(t(matrix(center_radius_df, nrow = 3)))
#'   
#'   return(list('errors_NS' = error_bands_NS, 'errors_EW' = error_bands_EW,
#'                'center_points_list_distance' = center_radius_list,
#'                'center_points_df_distance' = center_radius_df))
#' }


#' Estimating uncertainty of bubble CI
#' 
#' @description
#' This function takes in a list of dataframes, representing the different CI 
#' bubbles built on the track of the most "central" points, and extract center 
#' points, radiuses and extreme of the CI bubbles per each step
#' 
#' @param bubble_steps_CI list of dataframes, with each dataframe representing 
#' one step in the track of the most central TC
#' @param long_col Column index of the longitude
#' @param lat_col Column index of the latitude
#' @param unit_measure Unit measure of the distances 
#' 
#' @output List with centers, radius, positives and negatives dataframes
error_bands_bubbleCI <- function(bubble_steps_CI, long_col = 1, lat_col = 2, 
                                         unit_measure = "nautical mile") {

  # Calculating centers and radius
  centers <- sapply(bubble_steps_CI,function(df) df[1,]) %>% 
                        t() %>%
                        data.frame() %>% 
                        dplyr::select(long,lat) %>%
                        sapply(function(x) matrix(x, ncol = 1))

  centers <- data.frame(long = as.numeric(centers[, long_col]),
                        lat = as.numeric(centers[, lat_col])) 
                        #^ dataframe in list column problem
  radius <- sapply(bubble_steps_CI,function(df) max(df$distance_vec))
  
  # Calculating positive and negative diretions
  out_mat <- distAlongP(data_df = centers)
  bearing <- out_mat[[2]]
  bearing_orthog <- bearing + 90
  
  positive <- matrix(0,nrow = nrow(centers), ncol = 2 )
  negative <- matrix(0,nrow = nrow(centers), ncol = 2 )
  
  for (i in 1:nrow(centers)) {
    radius_ind <- datamart::uconv(radius[i], from = unit_measure, 
                                  to = "m", "Length")
    direction <- bearing_orthog[i]
    positive[i,] <- destPoint(centers[i,], direction, radius_ind)
    negative[i,] <- destPoint(centers[i,], direction, -radius_ind)
  }
  
  positive <- positive[-nrow(positive),]
  negative <- negative[-nrow(negative),]
  
  return(list(centers = centers,
              radius = radius,
              positive = positive,
              negative = negative)) 
}


#' Estimating Area of the Bubble CI
#' 
#' @description
#' Finds area of bubble CI, for each step, then takes the maximum
#' 
#' @details
#' This functions uses as inputs the output of error_bands_bubbleCI_posneg.
#' 
#' @param tc_bubble_structure Bubble CI list with centers, radius, positive 
#' and negative parts
#' 
#' @output List with the area of the Bubble CI for each step and the 
#' maximum of such values.
#' 
max_cumulative_area <- function(tc_bubble_structure) {

  # Checking whether the negative attributes are in the correct format
  # Positive are assumed to be the same as negative

  if (!(is.data.frame(tc_bubble_structure$negative)) & 
      !(is.matrix(tc_bubble_structure$negative))) {
    return(list(area_vec = 0, max = 0))
  }
  
  neg <- tc_bubble_structure$negative
  pos <- tc_bubble_structure$positive
  n <- nrow(neg)
  neg_back <- neg[n:1,]
  
  area_vec <- c()
  for (i in 2:nrow(neg)) {
    dat_all <- rbind(pos[1:i,],
                     neg_back[(n - i):n,])
    spPoly <- sp::SpatialPolygons(list(Polygons(list(Polygon(dat_all)),ID = 1)))
    area_vec <- c(area_vec, rgeos::gArea(spPoly))
  }  
  return(list(area_vec = area_vec, max = max(area_vec)))
}


#' Check if points are inside bubble CI
#' 
#' @description
#' Given a dataframe of points this function returns a vector of the same
#' length with 1 or 0 whether the respective points is included in any of the CI
#' bubble.
#' 
#' @param df_points Dataframe of points which inclusion needs to be calculated
#' @param center_df Dataframe of central points of the CI bubble
#' @param radius_df Dataframe of radius of the CI bubble
#' @param long column index of the longitude in the central points
#' @param lat column index of the latitude in the central points
#' 
#' @output Boolean vectors on whether the points are in any of the bubble CIs 
#' or not
check_points_in_bubbleCI <- function(df_points, center_df, radius_df, 
                                     long = 1, lat = 2){

  radius_df[is.na(radius_df)] <- 0
  center_df[is.na(center_df)] <- 0
  center_radius_df <- cbind(center_df[, c(long,lat)], radius_df)
  n_df <- dim(df_points)[1]
  in_vec <- numeric(n_df)
  
  for (i in c(1:n_df)) {

    for (c in c(1:dim(center_radius_df)[1])) {
      dist.measured <- geosphere::distGeo(
                        as.numeric(center_radius_df[c, c(1,2)]), 
                        as.numeric(df_points[i, c(1,2)]))

      if (abs(dist.measured) <= abs(center_radius_df[c, 3]) || 
        ((df_points[i, 1] == center_radius_df[c, 1]) &&
         (df_points[i, 2] == center_radius_df[c, 2])) ) {
        in_vec[i] <- 1
      }
    }
  }  
  return(in_vec)
}


#' Creating bubble CI out of TC simulated lists
#' 
#' @description
#' This function is a wrapper to determine CI bubble for list of simulated TCs
#' 
#' @param dflist List of simulated TCs
#' @param center_idx Index of the path that needs to be considered as a center
#' @param alpha_level Alpha level of the bubble CI
#' @param long Column index of the longitude
#' @param lat Column index of the latitude
#' @param unit_measure Unit of measure used for distance
bubble_ci_from_tclist <- function(dflist, center_idx, alpha_level = 0.1, 
                                 long = 1, lat = 2, 
                                 unit_measure = 'nautical mile') {

    bubble_steps_CI <- bubbleCI(dflist = dflist, center_idx = center_idx, 
                                alpha_level = alpha_level, 
                                long = long, lat = lat, 
                                output_length = unit_measure)

    bubble_ci_structure <- error_bands_bubbleCI(
                                  bubble_steps_CI = bubble_steps_CI, 
                                  long_col = long, lat_col = lat, 
                                  unit_measure = unit_measure)

    area_list <- max_cumulative_area(bubble_ci_structure)

    return(list('bubble_CI_object' = bubble_ci_structure, 
                'area' = area_list[[2]], 
                'area_vector' = area_list[[1]]))
} 


