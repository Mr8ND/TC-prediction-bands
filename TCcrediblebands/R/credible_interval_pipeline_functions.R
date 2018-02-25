#' Extract credible intervals of 4 methods from a single test TC
#' 
#' @description
#' This function extracts the KDE, Bubble CI, delta-ball and convex hull 
#' credible intervals from the simulations of a single TC. It then checks the 
#' coverage of such CI as number of points included in the CI for each of the 
#' methods
#' 
#' @param dflist List of simulated TCs
#' @param test_true_path Dataframe with the true TC path to be tested
#' @param alpha_level Alpha level of all the CI
#' @param long Column index of the longitude
#' @param lat Column index of the latitude
#' @param kde_grid_size Two dimensional vector for the KDE grid size
#' @param unit_measure Unit of measure used for distance
#' @param verbose If TRUE update messages will be displayed
#' 
#' @return 1 list with 4 arguments, once for each of the 4 methods, which all 
#' have an 'area' and 'in_vec' argument to determine the area value and a 
#' boolean vector which reported whether the point of the true TC was inside the
#' TC or not.
#' @export
credible_interval_single_tc <- function(dflist, test_true_path, alpha_level, 
                                        long = 1, lat = 2, verbose = FALSE,
                                        kde_grid_size = rep(1000,2),
                                        unit_measure = 'nautical mile'){

    # KDE CI
    kde_ci_list <- kde_from_tclist(dflist = dflist, 
    								grid_size = kde_grid_size,
                                    alpha_level = alpha_level)
    kde_predict_mat <- predict_kde_object(kde_obj = kde_ci_list$kde_object, 
                        predict_mat = test_true_path, 
                        alpha_level = alpha_level, 
                        long = long, lat = lat)

    out_kde_list <- list('contour' = kde_ci_list$contour, 
                          'area' = kde_ci_list$area,
                          'in_vec' = kde_predict_mat[, 4])

    # Distance Matrix calculation
    dflist_13pointsreduction <- thirteen_points_listable(dflist, 
                                                    c_position = c(long,lat),
                                                    verbose = verbose)
    dist_matrix_13pointsreduction <- distMatrixPath_innersq(
                                                dflist_13pointsreduction,
                                                output_length = unit_measure,
                                                verbose = verbose
                                                )
    depth_vector <- depth_function(dist_matrix_13pointsreduction)
    depth_vector_idx <- which(depth_vector == max(depth_vector))[1]

    # Bubble CI
    bubble_ci_list <- bubble_ci_from_tclist(dflist = dflist, 
                                      center_idx = depth_vector_idx, 
                                      alpha_level = alpha_level,
                                      long = long, lat = lat, 
                                      unit_measure = unit_measure)
    bubble_ci_inclusion_vec <- check_points_in_bubbleCI(
                                            df_points = test_true_path, 
                                            center_df = bubble_ci_list$bubble_CI_object$centers, 
                                            radius_df = bubble_ci_list$bubble_CI_object$radius, 
                                            long = long, lat = lat)

    out_bubble_list <- list(
                        'bubble_structure' = bubble_ci_list$bubble_CI_object,
                        'area' = bubble_ci_list$area,
                        'area_vector' = bubble_ci_list$area_vector,
                        'in_vec' = bubble_ci_inclusion_vec
                        )
    
    # deep points calculation
    data_deep_points <- depth_curves_to_points(data_list = dflist,
                                    alpha = alpha_level, 
                                    dist_mat = dist_matrix_13pointsreduction, 
                                    c_position = c(long,lat),
                                    depth_vector = depth_vector,
                                    verbose = verbose)

    # Delta Ball CI
    delta_ball_structure <- delta_structure(data_list = dflist, 
                                    alpha = alpha_level, 
                                    dist_mat = dist_matrix_13pointsreduction,
                                    data_deep_points = data_deep_points, 
                                    depth_vector = depth_vector,
                                    c_position = c(long,lat),
                                    area_ci_n = 2000, 
                                    area_ci_alpha = alpha_level, 
                                    verbose = verbose)

    delta_ball_inclusion_vec <- delta_ball_prop_interior(data_deep_points, 
                                                    test_true_path, 
                                                    delta_ball_structure$delta)

    out_delta_ball_list <- list(
        'structure' = delta_ball_structure$structure,
        'area' = delta_ball_structure$area,
        'area_ci' =  delta_ball_structure$area_ci,
        'delta' = delta_ball_structure$delta,
        'in_vec' = as.numeric(delta_ball_inclusion_vec[,1])
        )


    # Convex Hull CI
    convex_hull_structure <- convex_hull_structure(data_list = dflist, 
                                    alpha = alpha_level, 
                                    dist_mat = dist_matrix_13pointsreduction,
                                    data_deep_points = data_deep_points, 
                                    depth_vector = depth_vector,
                                    c_position = c(long,lat),
                                    verbose = verbose)

    convex_hull_inclusion_vec <- points_in_spatial_polygon(
                            convex_hull_structure$spPoly, 
                            as.matrix(test_true_path), 
                            long = long, lat = lat)

    out_convex_hull_list <- list(
        'structure' = convex_hull_structure$poly_df, 
        'area' = convex_hull_structure$area, 
        'spPoly' = convex_hull_structure$spPoly,
        'in_vec' = as.numeric(convex_hull_inclusion_vec)
        )


    return(list('kde' = out_kde_list, 
                'bubble_ci' = out_bubble_list,
                'delta_ball' = out_delta_ball_list, 
                'convex_hull' = out_convex_hull_list,
                'depth_vector' = depth_vector))
}


#' Extract CI from the full list of simulated and true TCs
#' 
#' @description
#' This function iterates through different type of curves and all the simulated curves
#' in order to obtain the credible intervals from the 4 different methods
#' 
#' @details
#' This function is just an iterative wrapper around the function credible_interval_single_tc
#' 
#' @param tc_full_sim_list List of simulated TCs
#' @param tc_true_path_list Dataframe with the true TC path to be tested
#' @param alpha_level Alpha level of all the CI
#' @param long Column index of the longitude
#' @param lat Column index of the latitude
#' @param start_idx Starting index when selecting which TC to be simulated - if NULL, defaulted
#' to 1
#' @param end_idx Ending index when selecting which TC to be simulated - if NULL, defaulted to
#' the number of TCs available
#' @param kde_grid_size Two dimensional vector for the KDE grid size
#' @param unit_measure Unit of measure used for distance
#' @param verbose if TRUE, an update on the process will be printed
#' @param curve_type_vec vector with the strings referring to the different types of curves
#' 
#' @return A list in which each sublist is a curve type and each sublist has for each element
#' a TC with the 4 different calculated credible intervals.
#' @export
credible_interval_pipeline <- function(tc_full_sim_list, tc_true_path_list, alpha_level = 0.1,
                                        start_idx = NULL, end_idx = NULL, long = 1, lat = 2, 
                                        unit_measure = 'nautical mile', verbose = TRUE,
                                        kde_grid_size = rep(1000,2),
                                        curve_type_vec = c('Auto_DeathRegs', 'Auto_NoDeathRegs', 
                                                           'NoAuto_DeathRegs', 'NoAuto_NoDeathRegs')) {
    output = list()
    list_tc_names = names(tc_full_sim_list)

    # Setting starting and ending index
    if (is.null(start_idx)){
        start_idx <- 1
    }
    if (is.null(end_idx)) {
        end_idx <- length(tc_true_path_list)
    }

  	if (verbose) {
	    pb <- progress::progress_bar$new(
	      format = "Credible Interval Pipeline [:bar] :percent eta: :eta",
	      total = length(c(start_idx:end_idx))*4, clear = FALSE, width = 51)
  	}

    for (j in c(start_idx:end_idx)) {
      
      # Retrieving simulated and true curves
      output_tc <- list()
      tc_sim_list_curve <- tc_full_sim_list[[list_tc_names[j]]]
      tc_true_path_curve <- tc_true_path_list[[list_tc_names[j]]]
      
      for (curve_type in curve_type_vec) {
        output_tc[[curve_type]] <- credible_interval_single_tc(dflist = lapply(tc_sim_list_curve[[curve_type]], data.frame), 
                                                         test_true_path = data.frame(tc_true_path_curve),
                                                         alpha_level = alpha_level,
                                                         long = long, lat = lat, 
                                                         unit_measure = unit_measure,
                                                         verbose = FALSE,
                                                         kde_grid_size = kde_grid_size)
        if (verbose) {
        pb$tick()
      	}
      }
      output[[j]] <- output_tc
    }
    
    return(output)
}

