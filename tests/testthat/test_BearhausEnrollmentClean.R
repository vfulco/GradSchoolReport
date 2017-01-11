context("BearhausEnrollmentClean")

test_that("BearhausEnrollmentClean converts data.frame", {
  expect_equal(BearhausEnrollmentClean.data.frame(iris), iris)
})

test_that("BearhausEnrollmentClean converts data.frame", {
  expect_equal(BearhausEnrollmentClean(list(iris, iris)), rbind(iris, iris))
})

