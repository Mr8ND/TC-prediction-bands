context("ggplot Visual Functions")

suppressWarnings(library(TCpredictionbands))


#' data_plot_paths_basic function -----------------------------------------

test_list <- list()
test_list[[1]] <- cbind(rep(0, 10), rep(0, 10))
test_list[[2]] <- cbind(rep(1, 10), rep(1, 10))
test_list[[3]] <- cbind(rep(2, 10), rep(2, 10))

df_plot <- TCpredictionbands::data_plot_paths_basic(test_list = test_list)
names_col <- c('lat', 'long', 'curve')

test_that("Basic structure is correct", {
  expect_equal(dim(df_plot), c(30, 3))
})

test_that("Test column names is correct", {
  expect_equal(names(df_plot), names_col)
})

test_that("Testing insertion is done correctly", {
  expect_equal(df_plot[c(1:10), 1], rep(0, 10))
  expect_equal(df_plot[c(11:20), 2], rep(1, 10))
  expect_equal(df_plot[c(21:30), 3], rep(3, 10))
})


#' contour_list_to_df function -------------------------------------------

cont_temp <- list()

cont_temp[[1]] <- list()
cont_temp[[1]]$x <- c(0, 1, 1, 0)
cont_temp[[1]]$y <- c(0, 0, 1, 1)
cont_temp[[1]]$level <- 1

cont_temp[[2]] <- list()
cont_temp[[2]]$x <- c(1, 2, 2, 1)
cont_temp[[2]]$y <- c(1, 1, 2, 2)
cont_temp[[2]]$level <- 2

cont_list_to_df <- TCpredictionbands::contour_list_to_df(cont_temp)

test_that("Dimension is correct", {
  expect_equal(length(cont_list_to_df), 2)
  expect_equal(dim(cont_list_to_df[[1]]), c(4,3))
})

test_that("Insertion is correct", {
  expect_equal(cont_list_to_df[[1]][, 1], cont_temp[[1]]$x)
  expect_equal(cont_list_to_df[[1]][, 2], cont_temp[[1]]$y)
  expect_equal(cont_list_to_df[[1]][, 3], rep(cont_temp[[1]]$level, 4))
  
  expect_equal(cont_list_to_df[[2]][, 1], cont_temp[[2]]$x)
  expect_equal(cont_list_to_df[[2]][, 2], cont_temp[[2]]$y)
  expect_equal(cont_list_to_df[[2]][, 3], rep(cont_temp[[2]]$level, 4))
})

