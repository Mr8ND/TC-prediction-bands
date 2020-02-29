context("KDE functions")

suppressWarnings(library(ks))

# flatten_df function -------------------------------------------

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


# extract_countour function --------------------------------------

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

test_that("test extract_countour - basic static example", {
  # basic example - 1 point
  df <- data.frame(x = c(0),
                   y = c(0))
  kde_object <- ks::kde(df,H = diag(2))
  cont <- extract_countour(kde_object, .05)
  
  cont_df <- as.data.frame(cont[[1]][c("x","y")])
  
  testthat::expect_equal(length(cont), 1) # single component
  testthat::expect_equivalent(cont_df[1,], cont_df[5,]) # full contour loop
  testthat::expect_true(all.equal(cont_df[1:4,] %>% 
                                    sapply(mean) %>% as.vector(),
                                  c(0,0))) # center at 0
})

# kde_contour_area function -------------------------------------

cont_temp <- list()
cont_temp$x <- c(0, 1, 1, 0)
cont_temp$y <- c(0, 0, 1, 1)
cont_area <- TCpredictionbands:::kde_contour_area(cont_temp)

test_that("Area of contour is calculated correctly", {
  expect_equal(cont_area, 1)
})


# points_in_contour function ------------------------------------

x1 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
y1 <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
predict_mat <- cbind(x1,y1)

position_wrt_contour <- TCpredictionbands::points_in_contour(
  cont_temp, predict_mat)

test_that("Points that are in the contour are correctly identified as such", {
  expect_equal(position_wrt_contour, rep(1, length(y1)))
})


# fit_kde_object function ------------------------------------

test_that("test fit_kde_object - basic static example", {
  # basic example - 1 point
  set.seed(8192)
  x <- 2^rnorm(100)
  y <- rnorm(100)
  dfmat <- cbind(x,y)
  
  kde_object <- fit_kde_object(dfmat)
  
  # just checking attributes relative to parameter inputs
  # (this function is really just a wrapper of ks...)
  testthat::expect_equal(kde_object$eval.points[[1]] %>% length(), 1000)
  testthat::expect_equal(kde_object$x, dfmat)
  testthat::expect_equal(kde_object$H, ks::Hpi(dfmat))

})

# kde_from_tclist function ------------------------------------
test_that("test kde_from_tclist - checking against a list - only contour", {
  data_list <- lapply(1:5, function(x) data.frame(x = 2^rnorm(100),
                                                  y = rnorm(100)))
  
  contour_list <- kde_from_tclist(data_list, alpha = .1)
  
  data_single <- data_list %>% do.call(rbind, .)
  kde_single <- ks::kde(data_single, gridsize = rep(1000,2))
  contour_single_df<- extract_countour(kde_single, alpha = .1)
  
  testthat::expect_equal(contour_list$contour, contour_single_df)
})
