context("Delta Ball Functions")


# get_delta function ------------------------------------------------

df_temp <- data.frame(cbind(c(0, 1, 1, 0), c(0, 0, 1, 1)))
# square (0,0), (1,0), (1,1), (0,1) - area should be 1

delta_out_temp <- TCpredictionbands::get_delta(df_temp)

test_that("Dimension is correct and matrix has diagonal equal 0", {
  expect_equal(dim(delta_out_temp$dist_mat), c(4,4))
  expect_equal(as.numeric(base::diag(delta_out_temp$dist_mat)), c(0,0,0,0))
})
  
test_that("Delta is equal to 1", {
  expect_equal(delta_out_temp$mm_delta, 1)
})


test_that("get_delta basic tests", {
  # static
  data <- data.frame(x = c(0,1), y = c(0,1))
  dist_mat <- matrix(c(0,sqrt(2),
                       sqrt(2),0), byrow = T, nrow = 2)
  gd_out1 <- get_delta(data)
  gd_out2 <- get_delta(dist_mat = dist_mat)
  
  testthat::expect_equivalent(gd_out1,gd_out2)
  testthat::expect_equivalent(gd_out1, list("dist_mat" = dist_mat,
                                            "mm_delta" = sqrt(2)))
  # static 2
  data2 <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
  dist_mat2 <- matrix(c(0,1,sqrt(2),1,
                        1,0,1,sqrt(2),
                        sqrt(2),1,0,1,
                        1,sqrt(2),1,0), byrow = T, nrow = 4)
  gd_out3 <- get_delta(data2)
  gd_out4 <- get_delta(dist_mat = dist_mat2)
  
  testthat::expect_equivalent(gd_out3, gd_out4)
  testthat::expect_equivalent(gd_out3, list("dist_mat" = dist_mat2,
                                            "mm_delta" = 1))
  # error
  testthat::expect_error(get_delta())
})


# depth_function ----------------------------------------------------
test_that("basic test for depth_function", {
  dist_mat <- matrix(c(0,   1, 1.5,
                       1,   0, 2,
                       1.5, 2, 0   ),
                     nrow = 3,
                     byrow = TRUE)
  
  dd_vec <- depth_function(dist_mat)
  
  testthat::expect_equal(dd_vec, c(1,0,0))
  
  dist_mat_not_sym <- matrix(c(0,   1, 0,
                               1,   0, 2,
                               1.5, 2, 0   ))
  dist_mat_not_pos <- matrix(c(0,   -1, 1.5,
                               -1,   0, 2,
                               1.5, 2, 0   ))
  
  testthat::expect_error(depth_function(dist_mat_not_sym))
  testthat::expect_error(depth_function(dist_mat_not_pos))
  
})

# get_box_points function -------------------------------------------

box_point_temp <- TCpredictionbands::get_box_points(df_temp, n=200)
test_that("Dimension is correct and size is correct", {
  expect_equal(dim(box_point_temp$box_points), c(200, 2))
  expect_equal(box_point_temp$size, 1)
})

df_temp2 <- data.frame(cbind(c(0, 2, 2, 0), c(0, 0, 2, 2)))
# square (0,0), (2,0), (2,2), (0,2) - area should be 4
box_point_temp2 <- TCpredictionbands::get_box_points(df_temp2, n=500)
test_that("Dimension is correct and size is correct", {
  expect_equal(dim(box_point_temp2$box_points), c(500, 2))
  expect_equal(box_point_temp2$size, 4)
})


# remove_duplicates_func --------------------------------------------
df_temp_dup <- data.frame(cbind(rep(1, 100), rep(5, 100)))
names(df_temp_dup) <- c("lat", "long")

dup_removed_df <- TCpredictionbands:::remove_duplicates_func(df_temp_dup)

test_that("Dimension and values are correct", {
  expect_equal(dim(dup_removed_df), c(1, 2))
  expect_equal(dup_removed_df[1,1], 1)
  expect_equal(dup_removed_df[1,2], 5)
})


# remove_delta_off_line_tests ----------------------------------------

line <- data.frame(x = c(0,1),
                   y = c(0,0)) %>%
  as.matrix()

delta = 1/4

out <- remove_delta_off_line(line, delta)
expected_out <- data.frame(x = c(.25,.75),
                           y = c(0,0)) %>%
  as.matrix()

test_that("remove_delta_off_line tests - basic in just x", {
  testthat::expect_equal(out, expected_out)
})

line <- data.frame(x = c(0,1),
                   y = c(0,1)) %>%
  as.matrix()

delta = sqrt(2)/4

out <- remove_delta_off_line(line, delta)
expected_out <- data.frame(x = c(.25,.75),
                           y = c(.25,.75)) %>%
  as.matrix()

test_that("steps_along_2d_line tests - basic in both dimensions", {
testthat::expect_equal(out, expected_out)
})


# remove_delta_off_line tests ----------------------------------------

#straight line
my_mat <- data.frame(x = c(1,20), y = c(1,20)) %>% 
  as.matrix()
test_that("basic tests for steps_along_2d_line - 45 degree line",{
  for (num_splits in sample(5:25,size = 5)){
    my_df_compression <- steps_along_2d_line(my_mat, n_steps = num_splits)
    
    testthat::expect_equal(nrow(my_df_compression), num_splits + 1)
    
    testthat::expect_equal(diff(my_df_compression[,1]),
                           rep(19/(num_splits), num_splits))
    
    testthat::expect_equal(my_df_compression[,1], my_df_compression[,2])
  }
})

# default n_steps
test_that(paste("basic tests for steps_along_2d_line - 45 degree line,",
                "default num steps"),{
  testthat::expect_equal(dim(steps_along_2d_line(my_mat)), c(101,2))
})

