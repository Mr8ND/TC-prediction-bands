context("Read Data Source")

suppressWarnings(library(TCpredictionbands))

#' convert_lat ---------------------------------------------------

df_temp_n <- data.frame(rep("10N", 100))
names(df_temp_n) <- "lat"
df_conversion_n <- TCpredictionbands:::convert_lat(df_temp_n)
test_that("Verify dimension and values are correct", {
  expect_equal(length(df_conversion_n), 100)
  expect_equal(as.numeric(df_conversion_n), rep(10, 100))
})

df_temp_s <- data.frame(rep("10S", 100))
names(df_temp_s) <- "lat"
df_conversion_s <- TCpredictionbands:::convert_lat(df_temp_s)
test_that("Verify dimension and values are correct", {
  expect_equal(length(df_conversion_s), 100)
  expect_equal(as.numeric(df_conversion_s), rep(-10, 100))
})


#' convert_long --------------------------------------------------

df_temp_e <- data.frame(rep("10E", 100))
names(df_temp_e) <- "long"
df_conversion_e <- TCpredictionbands:::convert_long(df_temp_e)
test_that("Verify dimension and values are correct", {
  expect_equal(length(df_conversion_e), 100)
  expect_equal(as.numeric(df_conversion_e), rep(10, 100))
})

# Testing also the case when longitude is below -180 degrees
df_temp_e2 <- data.frame(rep("-200E", 100))
names(df_temp_e2) <- "long"
df_conversion_e2 <- TCpredictionbands:::convert_long(df_temp_e2)
test_that("Verify dimension and values are correct", {
  expect_equal(length(df_conversion_e2), 100)
  expect_equal(as.numeric(df_conversion_e2), rep(160, 100))
})


df_temp_w <- data.frame(rep("10W", 100))
names(df_temp_w) <- "long"
df_conversion_w <- TCpredictionbands:::convert_long(df_temp_w)
test_that("Verify dimension and values are correct", {
  expect_equal(length(df_conversion_w), 100)
  expect_equal(as.numeric(df_conversion_w), rep(-10, 100))
})

