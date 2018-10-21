#' Find delta for covering
#' 
#' @description Find the minimum distance (delta) such that all points are 
#' within delta of at least one other point.
#'
#' @param data continuous data frame of individual points (in each row)
#' @param dist_mat distance matrix, calculated otherwise via euclidean distance
#'
#' @return 
#' \describe{
#' \item{dist_mat}{distance matrix between points}
#' \item{mm_delta}{the minimum distance (delta)}
#' }
#' @export
get_delta <- function(data, dist_mat = NULL){

  if (is.null(dist_mat)) {
      dist_mat <- as.matrix(stats::dist(data))
  }
  diag(dist_mat) <- max(dist_mat)
  mm_delta <- apply(dist_mat, MARGIN = 1, min ) %>% max
  
  # correct diagonals:
  diag(dist_mat) <- 0
  return(list(dist_mat = dist_mat, mm_delta = mm_delta))
}


#' Sample from box around data points
#' 
#' @description Inner function to get uniform sampling of points in box around 
#' data points
#'
#' @param data continuous data frame of individual points (in each row)
#' @param n number of points drawn
#'
#' @return 
#' \describe{
#' \item{box_points}{data frame of uniformly randomly drawn points}
#' \item{size}{the size of the box from which points are drawn}
#' }
#' 
#' @export
get_box_points <- function(data, n = 10000){
  # get points drawn uniformly from box around data points
  ranges <- sapply(data, range)
  
  box_points <- data.frame(x = stats::runif(n = n,
                                     min = ranges[1,1],
                                     max = ranges[2,1]),
                           y = stats::runif(n = n,
                                     min = ranges[1,2],
                                     max = ranges[2,2])
  )
  size <- diff(ranges[,1]) * diff(ranges[,2])
  return(list(box_points = box_points, size = size))
}

#' Estimate area of union of balls (inner)
#' 
#' @description Inner function to estimate the area of union of balls with 
#' radius delta, using uniform draws 
#'
#' @param data data continuous data frame of individual points (in each row)
#' @param query data points uniformly drawn from a space (with calculated size)
#' @param size size of space where query points were drawn
#' @param delta radius of each ball inside the union
#' @param alpha alpha level for 2 sided confidence interval estimate
#'
#' @return 
#' \describe{
#' \item{area}{estimated area of union of balls}
#' \item{area_ci}{vector with lower and upper confidence interval estimate}
#' }
get_area_inner <- function(data, query, size, delta, alpha = .05){
  n = nrow(query)
  neighbor <- RANN::nn2(data = data, query = query,
                  k = 1, treetype = "kd")
  
  prop <- mean(neighbor$nn.dists < delta)
  prop_ci <- prop + c(-1,1) * stats::qnorm(1 - alpha/2) * sqrt( (1 - prop) * prop / n )
  
  area <- prop * size 
  area_ci <- prop_ci * size
  return(list(area = area, area_ci = area_ci))
}


#' Estimate area of union of balls (wrapper)
#' 
#' @description Wrapper function to estimate the area of union of balls with 
#' radius delta, using uniform draws 
#'
#' @param data data continuous data frame of individual points (in each row)
#' @param delta radius of each ball inside the union
#' @param n number of points drawn uniformly within a box around the true data
#' @param alpha alpha level for 2 sided confidence interval estimate
#'
#' @return 
#' \describe{
#' \item{area}{estimated area of union of balls}
#' \item{area_ci}{vector with lower and upper confidence interval estimate}
#' }
#' @export
get_area <- function(data, delta, n = 10000, alpha = .05){
  
  unif_points <- get_box_points(data, n = n)
  query <- unif_points$box_points
  size  <- unif_points$size
  
  area_info <- get_area_inner(data, query, size, delta, alpha = alpha)
    
  return(area_info)
}

#' Make triangle matrix
#' 
#' @description Makes triangle matrix for points in matrix 
#'
#' @param dtri_data_tri sp object with triangles (n triangles)
#'
#' @return a matrix (n x 3) with strings of locations of 3 points in triangle 
#' @export
get_tri_matrix <- function(dtri_data_tri){
  # makes a matrix with string points of triangle (n x 3)
  num_tri <- length(dtri_data_tri@polygons)
  all_tri <- matrix("", nrow = num_tri, ncol = 3)
  
  for (idx_tri in 1:num_tri) {
    tri <- data.frame(
                  dtri_data_tri@polygons[[idx_tri]]@Polygons[[1]]@coords)[1:3,]
    all_tri[idx_tri,] <- tri %>% 
                          apply(1, function(x) paste0("(",x[1],",",x[2],")"))
  }
  
  return(all_tri)
}

#' Remove triangles with edges that need removal
#' 
#' @description Remove triangles from tuple matrix that have 1 or more edge that
#' needs to be removed
#'
#' TODO: this function needs to be cleaned up
#'
#' @param tuples_of_tri data frame with tuples of triangle edges and 
#' triangle index
#' @param removed_mat edges to be removed
#'
#' @return data frame with tuples of triangle not removed
remove_lines_from_tri <- function(tuples_of_tri, removed_mat){

  # Hack to make R CMD check not fail
  x <- y <- first <- second <- idx <- NULL

  # removes triangles for tuples data frame that have an edge removed 
  removed_mat <- removed_mat[apply(removed_mat, 1, 
                                   function(row) sum(is.na(row)) == 0), ]
  
  tuples_of_tri$combo <- apply(tuples_of_tri,1, 
                               function(row) paste0(row[1],"~",row[2]))
  
  removed_values_dat <- removed_mat %>%
    dplyr::group_by(idx) %>% dplyr::summarize(first = paste0("(",x[1],",", y[1],")"),
                                second = paste0("(",x[2],",", y[2],")"),
                                combo = paste0(first,"~",second),
                                combo2 = paste0(second,"~",first))
  
  removed_values_single <- c(removed_values_dat$combo, 
                             removed_values_dat$combo2)
  
  remove_tri <- tuples_of_tri$idx_tri[(
    tuples_of_tri$combo %in% removed_values_single
  )]
  
  out_tuples <- tuples_of_tri[!(tuples_of_tri$idx_tri %in% remove_tri),]
  
  return(out_tuples)
}

#' Shorten line by delta on both sides
#' 
#' @description Inner function to remove delta length from both sides of a line
#'
#' @param line 2 x 2 matrix of edge points of line
#' @param delta numeric delta to be subtracted
#'
#' @return 2 x 2 matrix of edges that are shrunk
remove_delta_off_line <- function(line, delta){
  # (inner function) removed delta length of each side of a line
  diffs <- diff(line)
  if (diffs[1] == 0) {
    x_shift <- 0
    y_shift <- delta
  } else {
    slope <- diffs[2]/diffs[1]
    
    x_shift <- sqrt(delta^2 / (slope^2 + 1))
    y_shift <- slope * x_shift
  }

  
  out_line <- line + matrix(c(x_shift,y_shift,
                              -x_shift,-y_shift), nrow = 2, byrow = T)
  return(out_line)
}

#' Create n equidistance points
#' 
#' @description Inner Function to create n equidistant points along a line
#'
#' @param line 2 x 2 matrix of edge points of line
#' @param n_steps integer number of steps (n)
#'
#' @return n x 2 matrix with points on path
steps_along_2d_line <- function(line, n_steps = 1000){
  # (inner function) finds equidistance points along a line
  len   <- stats::dist(line)
  diffs <- diff(line)
  
  if (diffs[1] == 0) {
    x_shift <- 0
    y_shift <- len
  } else {
    slope <- diffs[2]/diffs[1]
    
    x_shift <- sqrt(len^2/(slope^2 + 1))
    y_shift <- slope * x_shift
  }
  

  points <-  matrix(rep(line[1,], each = n_steps + 1), ncol = 2) + t(
    matrix(rep((1:(n_steps + 1) - 1) / n_steps , each = 2),
           nrow = 2, byrow = F) * c(x_shift,y_shift) ) 
  return(points)
}

#' Get edges within union of balls
#' 
#' @description Figure out which edges in the delaunay diagram are within the 
#' union of balls
#'
#' @param delaunay_tri_data sp data of delaunay triangles
#' @param data_raw data frame with center points of balls 
#' @param delta fixed radius of all the balls 
#' @param n_steps number of equidistance points along the line, past delta 
#' on both sides, that will be checked to approximate all points along the line
#'
#' @return 
#' \describe{
#' \item{lines_mat}{lines of edges that are kept (each edge has 2 rows and 
#' share an index). These edges are within the union of the balls.}
#' \item{removed_mat}{lines of edges that should be removed (i.e. are not 
#' with the union of balls.)}
#' }
#' @export
get_lines <- function(delaunay_tri_data, data_raw, delta, n_steps = 100){
  ## this function gets lines that are included within the balls
  
  #assert_that(class(delaunay_tri_data) == "SpatialLines")
  
  idv_lines <- delaunay_tri_data@lines[[1]]@Lines
  n <- length(idv_lines)
  
  lines_mat <- matrix(NA, nrow = 2*n, ncol = 2)
  
  removed_mat <- matrix(NA, nrow = 2*n, ncol = 2)
  
  for (idx in 1:n) {
    l <- idv_lines[[idx]]@coords
    
    if (stats::dist(l) > delta * 2) {
      l_inner <- remove_delta_off_line(l, delta)
      points_along <- steps_along_2d_line(l_inner,n_steps)
      neighbor <- RANN::nn2(data = data_raw, query = points_along,
                      k = 1, treetype = "kd")
      if (!all(neighbor$nn.dists < delta)) {
        removed_mat[(2*idx - 1):(2*idx),] <- l
        next
      }
    } 
    
    lines_mat[(2*idx - 1):(2*idx),] <- l
  }
  
  # cleaning up return
  
  colnames(lines_mat) <- c("x", "y")
  lines_mat <- data.frame(lines_mat)
  lines_mat$idx <- rep(1:(nrow(lines_mat)/2), each = 2)
  
  colnames(removed_mat) <- c("x", "y")
  removed_mat <- data.frame(removed_mat)
  removed_mat$idx <- rep(1:(nrow(removed_mat)/2), each = 2)
  
  return(list(lines_mat = lines_mat, removed_mat = removed_mat ))
}

#' Remove duplicate points
#' 
#' @description Remove duplicates of points in point cloud (needed for certain 
#' algorithms)
#'
#' @param data_raw data points of observations 
#' expected lat, long column names
#'
#' @return data without duplicates
remove_duplicates_func <- function(data_raw){

  # Hack to make R CMD Check not fail
  connectors <- lat <- long <- NULL

  data_raw$connectors <- apply(data_raw,1, function(row) paste0(row[1],",",row[2]))
  
  data_out <- data_raw %>% dplyr::group_by(connectors) %>%
    dplyr::summarize(lat = unique(lat),
              long = unique(long))
  data_out <- data.frame(data_out %>% dplyr::select(lat, long))
  
  return(data_out)
}

#' Run delta ball analysis 
#' 
#' \strong{See comment at bottom of function: this function I think does the 
#' incorrect alignment - effects all the way to \code{ggvis_delta_ball_contour}}
#' 
#' @description Run delta ball analysis and get outline of points. Runs all 
#' analyses to get dataframe with regular lines. Part of the approach in 
#' "Computing Polygonal Surfaces from Unions of Balls" by Tam and Heidrich.
#' 
#' @param data_raw data frame with center points of balls 
#' @param n_steps number of equidistance points along the line, past delta 
#' on both sides, that will be checked to approximate all points along the line
#' @param remove_duplicates boolean if need to remove duplicates in data_raw
#'
#' @return data frame of exterior lines (not ordered)
#' @export
delta_ball_wrapper <- function(data_raw, n_steps = 1000, remove_duplicates = F){
  
  if (remove_duplicates) {
    data_raw <- remove_duplicates_func(data_raw)
  }

  #Hack to set variables equal to NULL so that R CMD check does not flag them
  X1 <- X2 <- id <- idx <- idx_tri <- n <- NULL
  
  data <- data_raw[,1:2]
  sp::coordinates(data) <- names(data_raw)[1:2]
    
  # get delta value
  d <- get_delta(data_raw)
  delta <- d$mm_delta/2
  
  # create correct edges 
  dtri_data_edges <- rgeos::gDelaunayTriangulation(data, onlyEdges = T,tolerance = 0)
  
  lines_info <- get_lines(dtri_data_edges, data_raw, delta, n_steps = 100)
  
  desired_lines <- lines_info$lines_mat
  keep <- desired_lines %>% apply(MARGIN = 1, 
                                  function(row) sum(is.na(row)) == 0) 
  desired_lines <- desired_lines[keep,]
  
  removed_mat <- lines_info$removed_mat
  
  # string representation of nodes and edges 
  nodes <- paste0("(",desired_lines$x, ",", desired_lines$y, ")")
  edge_mat <- matrix(c(nodes[seq(from = 1,to = length(nodes),by = 2)],
                       nodes[seq(from = 2,to = length(nodes),by = 2)]),
                     ncol = 2) %>% 
    data.frame() %>% 
    dplyr::mutate(X1 = as.character(X1),
           X2 = as.character(X2),
           id = desired_lines$idx[seq(from = 1,to = length(nodes),by = 2)])
  
  # get DT triangles
  dtri_data_tri <- rgeos::gDelaunayTriangulation(data,tolerance = 0)
  tri_matrix <- get_tri_matrix(dtri_data_tri)
  
  tuples_of_tri <- data.frame(rbind(tri_matrix[,c(1,2)],
                                    tri_matrix[,c(1,3)],
                                    tri_matrix[,c(2,3)],
                                    # both directions
                                    tri_matrix[,c(2,1)],
                                    tri_matrix[,c(3,1)],
                                    tri_matrix[,c(3,2)]),
                              stringsAsFactors = F 
  ) %>%
    dplyr::mutate(idx_tri = rep(1:nrow(tri_matrix),times = 6))
  
  
  tuples_of_tri <- remove_lines_from_tri(tuples_of_tri = tuples_of_tri,
                                         removed_mat = removed_mat)
  
  # what type of edge are you?
  
  num_tri <- edge_mat %>% dplyr::left_join(tuples_of_tri,
                                    by = c("X1" = "X1", "X2" = "X2"))  %>%
    dplyr::group_by(id) %>% dplyr::summarise(idx_tri = paste0(idx_tri,collapse = ","),
                               X1 = unique(X1),
                               X2 = unique(X2),
                               count = n())
  
  # merging & getting regular lines --------------------
  
  index_mapping <- data.frame(dl = sort(unique(desired_lines$idx)),
                              nt = sort(unique(num_tri$id)))
  
  select_lines <- (num_tri[num_tri$count == 1, c("id")] %>% 
                     dplyr::left_join(index_mapping, by = c("id" = "nt")))$dl
  
  output_lines <- desired_lines %>% dplyr::filter(idx %in% select_lines)
  names(output_lines)[1:2] = c("long","lat") ###
  return(output_lines)
}

#' Performs delta ball approach
#'
#' @param data_list list of hurricanes
#' @param alpha for prediction band (related to depth). Takes value in (0, 1.0), 
#'        for a 95\% PB, set alpha to .05.
#' @param dist_mat distance matrix (otherwise is calculated)
#' @param data_deep_points data deep points from depth function 
#'        (otherwise calculated)
#' @param position Columns position of long/lat pair
#' @param depth_vector Vector with depth values
#' @param area_ci_n number of observations to estimate area of covering
#' @param area_ci_alpha alpha level for confidence interval of area of covering
#' @param verbose if the distance matrix is verbose
#' @param ... other parameters in distance calculation through 
#'        \code{\link{distMatrixPath_innersq}}
#'
#' @return 
#' \describe{
#' \item{structure}{data frame of non-ordered lines of contour}
#' \item{area}{estimated error of contour area (float)}
#' \item{area_ci}{area confidence interval (vector)}
#' \item{delta}{optimal delta for covering}
#' }
#' @export
delta_structure <- function(data_list, alpha, dist_mat = NULL, 
                            data_deep_points = NULL,
                            position = 1:2,
                            depth_vector = NULL,
                            area_ci_n = 2000, 
                            area_ci_alpha = .05, verbose = FALSE, ...){
  if (is.null(data_deep_points)) {
    # depth approach ---------------
    data_deep_points <- depth_curves_to_points(data_list, 
                                               alpha, 
                                               dist_mat = dist_mat,
                                               verbose = verbose,
                                               position = position,
                                               depth_vector = depth_vector,
                                               ...)
  }
  # getting the minimum delta ---------------
  
  d_out <- get_delta(data_deep_points)
  delta = d_out$mm_delta
  
  # area calculation ----------------
  area_info = get_area(data_deep_points, 
                       delta, n = area_ci_n, 
                       alpha = area_ci_alpha)
  
  # structure creation -----------------
  
  structure_df <- delta_ball_wrapper(data_deep_points, remove_duplicates = TRUE)
  
  out <- list()
  out[["structure"]] <- structure_df
  out[["area"]] <- area_info$area
  out[["area_ci"]] <- area_info$area_ci
  out[["delta"]] <- delta
  # maybe also return depths?
  
  return(out)
}

#' Proportion of true TC steps within delta-ball CB
#'
#' @param raw_data_points data continuous data frame of individual points 
#' (in each row)
#' @param truth_points data frame of true points
#' @param delta radius of each ball inside the union
#'
#' @return in_out_vec boolean vector if point is within delta of raw points
delta_ball_prop_interior <- function(raw_data_points, truth_points, delta){
  neighbor <- RANN::nn2(data = raw_data_points, query = truth_points,
                  k = 1, treetype = "kd")
  in_out_vec <- (neighbor$nn.dists < delta)
  
  return(in_out_vec)
}

