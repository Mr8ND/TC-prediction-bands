context("Delta Ball Functions")

suppressWarnings(library(TCpredictionbands))


#' get_delta function ------------------------------------------------

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


#' get_box_points function -------------------------------------------

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


#' remove_duplicates_func --------------------------------------------
df_temp_dup <- data.frame(cbind(rep(1, 100), rep(5, 100)))
names(df_temp_dup) <- c("lat", "long")

dup_removed_df <- TCpredictionbands:::remove_duplicates_func(df_temp_dup)

test_that("Dimension and values are correct", {
  expect_equal(dim(dup_removed_df), c(1, 2))
  expect_equal(dup_removed_df[1,1], 1)
  expect_equal(dup_removed_df[1,2], 5)
})

