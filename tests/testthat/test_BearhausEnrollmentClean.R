context("BearhausEnrollmentClean")

test_that("BearhausEnrollmentClean converts data.frame", {
  expect_equal(BearhausEnrollmentClean(iris, test = TRUE), iris)
})

test_that("BearhausEnrollmentClean converts data.frame", {
  expect_equal(BearhausEnrollmentClean(list(iris, iris), test = TRUE), rbind(iris, iris))
})

