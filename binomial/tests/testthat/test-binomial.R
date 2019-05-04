context("binomial")

test_that("test : check bin_choose works", {
  expect_equal(bin_choose(10, 5), 252)
  expect_length(bin_choose(10, 5), 1)
  expect_type(bin_choose(10, 5), "double")
  expect_equal(bin_choose(5, 1:3), c(5, 10, 10))
  expect_error(bin_choose(3, 4), "k cannot be greater than n")
})

test_that("test : check bin_probability works", {
  expect_equal(bin_probability(3, 5, 0.5), 0.3125)
  expect_equal(bin_probability(1:2, 5, 0.5), c(0.15625, 0.31250))
  expect_length(bin_probability(3, 5, 0.5), 1)
  expect_length(bin_probability(1:2, 5, 0.5), 2)
  expect_type(bin_probability(3, 5, 0.5), "double")
  expect_error(bin_probability(7, 4, -0.4))
})

test_that("test : check bin_distribution works", {
  expect_is(bin_distribution(3, 0.5), c("bindis", "data.frame"))
  expect_length(bin_distribution(3, 0.5), 2)
  expect_equal(bin_distribution(3, 0.5),
               data.frame(success = 0:3, probability = c(0.125, 0.375, 0.375, 0.125))
               )
  expect_error(bin_distribution(5,-1))
})

test_that("test : check bin_cumulative works", {
  expect_is(bin_cumulative(3, 0.5), c("bincum", "data.frame"))
  expect_length(bin_cumulative(3, 0.5), 3)
  expect_equal(bin_cumulative(5, 0.5),
               data.frame(success = 0:5,
                          probability = c(0.125, 0.375, 0.375, 0.125),
                          cumulative = c(0.125, 0.500, 0.875, 1.000)
                          )
               )
})
