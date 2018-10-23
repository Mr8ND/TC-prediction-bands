context("Regression Functions")

suppressWarnings(library(TCpredictionbands))

#' shift function ---------------------------------------------------

vec_temp_1 <- c(0,1,2,3,4)
vec_temp_1_shifted <- TCpredictionbands::shift(vec_temp_1)
test_that("Verify dimension and values are correct for first example", {
  expect_equal(length(vec_temp_1_shifted), length(vec_temp_1))
  expect_equal(vec_temp_1_shifted, c(NA, c(0,1,2,3)))
})

vec_temp_2 <- c(1:1000)
vec_temp_2_shifted <- TCpredictionbands::shift(vec_temp_2)
test_that("Verify dimension and values are correct for second example", {
  expect_equal(length(vec_temp_2_shifted), length(vec_temp_2))
  expect_equal(vec_temp_2_shifted, c(NA, c(1:999)))
})


#' get_east_west function -------------------------------------------

test_vec_dir <- c(0, 50, 100, 150, 180, 200, 250, 300, 360)
east_west_test_dir <- TCpredictionbands::get_east_west(test_vec_dir)
test_that("Verify dimension and values are correct", {
  expect_equal(length(east_west_test_dir), length(test_vec_dir))
  expect_equal(east_west_test_dir, c(rep("E", 4), rep("W", 5)))
})


#' round_curve function ---------------------------------------------

set.seed(8192)
x1 <- 2^rnorm(100)
y1 <- rnorm(100)
dfmat_temp <- cbind(x1,y1)
dfmat_temp_rounded <- TCpredictionbands::round_curve(dfmat_temp)

test_that("Verify dimension and values are correct", {
  expect_equal(dim(dfmat_temp_rounded), dim(dfmat_temp))
  expect_equal(dfmat_temp_rounded[,1], round(dfmat_temp[, 1], 1))
  expect_equal(dfmat_temp_rounded[,2], round(dfmat_temp[, 2], 1))
})