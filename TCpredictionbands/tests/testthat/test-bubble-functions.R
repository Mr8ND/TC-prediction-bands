context("Bubble Functions")

library(TCpredictionbands)

#' rearrange_dflist_bubble function  ---------------------------------------

internal_data <- TCpredictionbands:::internal_data
sample_sim <- internal_data[["sample_sim"]]
center_idx <- 100
sample_dflist_bubble <- TCpredictionbands:::rearrange_dflist_bubble(
                          sample_sim, center_idx)

test_that("Dimension is correct", {
  expect_equal(dim(sample_dflist_bubble[[1]])[1], length(sample_sim))
  expect_equal(dim(sample_dflist_bubble[[1]])[2], 2)
  expect_equal(length(sample_dflist_bubble), dim(sample_sim[[center_idx]])[1])
})

test_that("Verify some of the insertions are correct", {
  expect_equal(sample_dflist_bubble[[1]][1,1], 
               as.numeric(sample_sim[[1]][1,1]))
  expect_equal(sample_dflist_bubble[[3]][150,1], 
               as.numeric(sample_sim[[150]][3,1]))
  expect_equal(sample_dflist_bubble[[7]][200,2], 
               as.numeric(sample_sim[[200]][7,2]))
})


#' check_points_in_bubbleCI function  ---------------------------------------

df_points_test <- data.frame(cbind(c(1:20), c(2:21)))
center_df_test <- df_points_test
radius_df_test <- data.frame(rep(1, 20))

point_prediction_bubble <- TCpredictionbands:::check_points_in_bubbleCI(
  df_points_test, center_df_test, radius_df_test)

test_that("Point Prediction Bubble is correct", {
  expect_equal(length(point_prediction_bubble), 20)
  expect_equal(point_prediction_bubble, rep(1, 20))
})


