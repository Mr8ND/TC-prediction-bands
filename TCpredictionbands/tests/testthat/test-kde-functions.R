context("KDE functions")

suppressWarnings(library(TCpredictionbands))
suppressWarnings(library(ks))

#' flatten_df function -------------------------------------------

set.seed(8192)
len_dflist <- 10
dim_dataframe <- 100

dflist <- list()
for (i in c(1:len_dflist)) {
  x <- 2^rnorm(dim_dataframe)
  y <- rnorm(dim_dataframe)
  dflist[[i]] <- cbind(x,y)
}
flatten_df <- TCpredictionbands:::flatten_tc_list(dflist)

test_that("Dimension is the correct one", {
  expect_equal(dim(flatten_df)[1], dim_dataframe*len_dflist)
  expect_equal(dim(flatten_df)[2], 2)
})

test_that("Dataframe insertion was correct", {
  expect_equal(flatten_df[c(1:100),], dflist[[1]])
  expect_equal(flatten_df[c(101:200),], dflist[[2]])
})


#' extract_contour function --------------------------------------

x1 <- 2^rnorm(100)
y1 <- rnorm(100)
dfmat <- cbind(x1,y1)
kde_object <- ks::kde(dfmat)

cont_05 <- TCpredictionbands::extract_countour(kde_object, .05)
cont_1 <- TCpredictionbands::extract_countour(kde_object, .1)
cont_2 <- TCpredictionbands::extract_countour(kde_object, .2)

test_that("Level is selected correctly", {
  expect_equal(cont_05[[1]]$level, as.numeric(kde_object$cont["5%"]))
  expect_equal(cont_1[[1]]$level, as.numeric(kde_object$cont["10%"]))
  expect_equal(cont_2[[1]]$level, as.numeric(kde_object$cont["20%"]))
})


#' kde_contour_area function -------------------------------------

cont_temp <- list()
cont_temp$x <- c(0, 1, 1, 0)
cont_temp$y <- c(0, 0, 1, 1)
cont_area <- TCpredictionbands:::kde_contour_area(cont_temp)

test_that("Area of contour is calculated correctly", {
  expect_equal(cont_area, 1)
})


#' points_in_contour function ------------------------------------

x1 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
y1 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
predict_mat <- cbind(x1,y1)

position_wrt_contour <- TCpredictionbands::points_in_contour(
  cont_temp, predict_mat)

test_that("Points that are in the contour are correctly identified as such", {
  expect_equal(position_wrt_contour, rep(1, length(y1)))
})
