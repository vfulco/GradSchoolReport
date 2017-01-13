context("BearhausApplicationClean")

test_that("BearhausApplicationClean converts data.frame", {
  expect_equal(is.data.frame(BearhausApplicationClean.data.frame(iris)), is.data.frame(iris))
})

test_that("BearhausApplicationClean converts data.frame", {
  expect_equal(is.data.frame(BearhausApplicationClean(list(iris, iris))), is.data.frame(rbind(iris, iris)))
})

