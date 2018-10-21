context("Convex Hull - points in spatial polygon")

suppressWarnings(library(sp))
suppressWarnings(library(ks))

set.seed(8192)

# First creating a polygon
x1 <- 2^rnorm(100)
y1 <- rnorm(100)
dfmat <- cbind(x1,y1)
kde_object <- ks::kde(dfmat)
cont <- with(kde_object, contourLines(x = eval.points[[1]],
                                       y = eval.points[[2]],
                                       z = estimate,
                                       levels = cont["5%"])[[1]])
test_that("Contour level is picked up correctly", {
  expect_equal(as.numeric(cont$level), as.numeric(kde_object$cont["5%"]))
})
poly <- with(cont, data.frame(x,y))
poly <- rbind(poly, poly[1, ])    # polygon needs to be closed
spPoly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(poly)),ID = 1)))


x1 <- 2^rnorm(100)
y1 <- rnorm(100)
predict_mat <- cbind(x1,y1)
 
position_wrt_contour <- points_in_spatial_polygon(spPoly, predict_mat)

# Writing the vector explicitly rather than saving it for quicker use.
# Having the set seed, we expect always to be consistently it, unless
# we change the points_in_spatial_polygon function.
# Use paste(position_wrt_contour, collapse=", ") in the console if needed.
old_position_wrt_contour <- c(0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1,
                              0, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1,
                              0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0,
                              1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0,
                              0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1,
                              1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0)

test_that("Position with respect to points in polygon have not changed", {
  expect_identical(position_wrt_contour, old_position_wrt_contour)
})

