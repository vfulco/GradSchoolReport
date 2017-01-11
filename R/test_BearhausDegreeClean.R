context("BearhausDegreeClean")

test_that("BearhausDegreeClean converts data.frame", {
  expect_equal(BearhausDegreeClean.data.frame(iris), iris)
})

test_that("BearhausDegreeClean converts data.frame", {
  expect_equal(BearhausDegreeClean(list(iris, iris)), rbind(iris, iris))
})

