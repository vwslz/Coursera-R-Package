library(testthat)

test_that("make_filename", {
  expect_that(make_filename(2013), equals("accident_2013.csv.bz2"))
  expect_that(make_filename(2014), equals("accident_2014.csv.bz2"))
  expect_that(make_filename(2015), equals("accident_2015.csv.bz2"))
})

test_that("fars_read", {
  expect_that(fars_read(make_filename(2013)), is_a("data.frame"))
  expect_that(fars_read(make_filename(2014)), is_a("data.frame"))
  expect_that(fars_read(make_filename(2015)), is_a("data.frame"))
})

test_that("fars_read_years", {
  expect_that(fars_read_years(2013), is_a("list"))
  expect_that(fars_read_years(c(2013, 2014, 2015)), is_a("list"))
  expect_null(dim(fars_read_years(2016)))
})

test_that("fars_summarize_years", {
  expect_that(fars_summarize_years(2013), is_a("data.frame"))
  expect_that(fars_summarize_years(c(2013, 2014, 2015)), is_a("data.frame"))
})

test_that("fars_map_state", {
  expect_invisible(fars_map_state(12, 2013))
  expect_invisible(fars_map_state(1, 2015))
})
