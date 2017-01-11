context("read_csv")

test_that("read_csv reads_list and combines", {
  expect_equal(read_csv(list("a,b\n1.0,2.0", "a,b\n1.0,2.0"),
                        combine = TRUE),
               data.frame(a = c(1, 1), b = c(2, 2)))
})

test_that("read_csv reads_list and combines", {
  expect_equal(read_csv(list("a,b\n1.0,2.0", "a,b\n1.0,2.0"),
                        combine = FALSE),
               list(tibble::data_frame(a = c(1), b = c(2)),
                    tibble::data_frame(a = c(1), b = c(2))))
})


test_that("read_csv default", {
  expect_equal(read_csv("a,b\n1.0,2.0"),
               tibble::data_frame(a = c(1), b = c(2)))
})
