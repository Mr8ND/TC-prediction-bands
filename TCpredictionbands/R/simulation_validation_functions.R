#' Credible Band Inclusion on Simulated TCs
#' 
#' @description
#' This function takes a list of calculated credible bands and a series of simulated TCs
#' and calculates how many points of each of the simulated TCs are included in each of the
#' method's credible bands. The inclusion is done by returning 1 if a point is inside a
#' credible band and 0 if not.
#' 
#' @param hur_out_obj Object with the credible bands for each of the method. Assumed to be 
#' a list with at least the 4 names referring to the methods, i.e. 'kde', 'bubble_ci',
#' 'delta_ball' and 'convex_hull'
#' @param sim_hur_list List of simulated hurricanes. Each object is assumed to be a data.frame
#' @param long Column index of the longitude
#' @param lat Column index of the latitude
#' @param unit_measure Unit of measure used for distance
#' @param verbose If TRUE update messages will be displayed
#' 
#' @return One list with 4 arguments, one for each of the 4 methods, in which there is the same
#' number of vectors as the simulated TCs. Each vector is a binary vector, in which 1 indicates
#' that the point was included in the credible band and 0 if not.
#' @export 
calculate_invec_per_method <- function(hur_out_obj, sim_hur_list, long = 1, lat = 2,
									unit_measure = 'nautical mile', verbose = FALSE){
	# KDE
	kde_time_start <- Sys.time()
	cont_list <- hur_out_obj[["kde"]][["contour"]]
	kde_invec_list <- lapply(X = sim_hur_list,
							FUN = points_in_contour_list, 
							cont_list = cont_list, 
							long = long, lat = lat)
	kde_time_final <- Sys.time() - kde_time_start

	# BUBBLE CI
	bubble_time_start <- Sys.time()
	tc_bubble_structure <- hur_out_obj[["bubble_ci"]][["bubble_structure"]]
	bubble_invec_list <- lapply(X = sim_hur_list,
							FUN = check_points_within_diff_radius,
                            tc_bubble_structure = tc_bubble_structure,
                            long = long, lat = lat, 
                            unit_measure = unit_measure,
                            verbose = verbose)
	bubble_time_final <- Sys.time() - bubble_time_start

	# DELTA BALL COVERING
	delta_time_start <- Sys.time()
	deep_points <- hur_out_obj[["delta_ball"]][["deep_points"]]
	delta <- hur_out_obj[["delta_ball"]][["delta"]]
	delta_ball_invec_list <- lapply(X = sim_hur_list,
							FUN = delta_ball_prop_interior,
							raw_data_points = deep_points, 
                            delta = delta)
	delta_ball_invec_list <- lapply(X = delta_ball_invec_list, function(x) as.numeric(x[,1]))
	delta_time_final <- Sys.time() - delta_time_start

	# CONVEX HULL
	convex_time_start <- Sys.time()
	ch_polygon <- hur_out_obj[["convex_hull"]][["spPoly"]]
	sim_hur_list_mat <- lapply(sim_hur_list, as.matrix)
	ch_invec_list <- lapply(X = sim_hur_list_mat, 
							FUN = points_in_spatial_polygon,
							spPoly = ch_polygon,
							long = long, lat = lat)
	convex_time_final <- Sys.time() - convex_time_start

	return(list('kde' = kde_invec_list,
				'bubble_ci' = bubble_invec_list,
				'delta_ball' = delta_ball_invec_list,
				'convex_hull' = ch_invec_list,
				"time" = c("kde" = kde_time_final[1],
						   "bubble" = bubble_time_final[1],
						   "delta" = delta_time_final[1],
						   "convex" = convex_time_final[1])))
}