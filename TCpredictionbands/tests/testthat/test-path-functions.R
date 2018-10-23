context("Path Functions")

library(TCpredictionbands)

#' swap2DfCols function ---------------------------------

df_temp <- data.frame(cbind(rep(1, 100), rep(5, 100)))
df_temp2 <- data.frame(cbind(rep(5, 100), rep(10, 100), rep(15, 100)))

df_temp_swapped <- TCpredictionbands::swap2DfCols(df_temp)
test_that("Dimension and values are correct for the first example", {
  expect_equal(dim(df_temp_swapped), c(100, 2))
  expect_equal(df_temp_swapped, data.frame(cbind(rep(5, 100), rep(1, 100))))
})

df_temp_swapped2 <- TCpredictionbands::swap2DfCols(df_temp2)
test_that("Dimension and values are correct for the second example", {
  expect_equal(dim(df_temp_swapped2), c(100, 2))
  expect_equal(df_temp_swapped2,
               data.frame(cbind(rep(10, 100), rep(5, 100))))
})


