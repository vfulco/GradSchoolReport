context("read_excel")

iris$Species <- as.character(iris$Species)

test_that("read_excel reads_list and combines", {
  expect_equal(read_excel(list(system.file("extdata/datasets.xlsx", package = "readxl"),
                               system.file("extdata/datasets.xlsx", package = "readxl")),
                          combine = TRUE), rbind(iris, iris))
})

test_that("read_excel reads_list and combines", {
  expect_equal(read_excel(list(system.file("extdata/datasets.xlsx", package = "readxl"),
                               system.file("extdata/datasets.xlsx", package = "readxl")),
                          combine = FALSE), list(tibble::as_data_frame(iris), tibble::as_data_frame(iris)))
})
