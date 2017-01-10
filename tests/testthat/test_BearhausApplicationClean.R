context("BearhausApplicationClean")

test_that("BearhausApplicationClean converts data.frame", {
  expect_equal(BearhausApplicationClean.data.frame(iris), iris)
})

test_that("BearhausApplicationClean converts data.frame", {
  expect_equal(BearhausApplicationClean(iris), iris)
})

