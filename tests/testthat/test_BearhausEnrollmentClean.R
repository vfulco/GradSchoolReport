context("BearhausEnrollmentClean")

df <- simEnrollment(1)

test_that("BearhausEnrollmentClean converts data.frame", {
  expect_equal(BearhausEnrollmentClean(df), BearhausEnrollmentClean(df))
})

test_that("BearhausEnrollmentClean converts data.frame", {
  expect_equal(BearhausEnrollmentClean(list(df, df)), BearhausEnrollmentClean(list(df, df)))
})

