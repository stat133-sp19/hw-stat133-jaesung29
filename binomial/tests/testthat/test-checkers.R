context("test checker functions")

test_that("test : check_prob works", {
  expect_true(check_prob(0.7))
  expect_true(check_prob(0))
  expect_length(check_prob(0.7), 1)
  expect_error(check_prob(100), "prob must be a number betwen 0 and 1")
  expect_error(check_prob(-1), "prob must be a number betwen 0 and 1")
  expect_type(check_prob(0.5), "logical")
})

test_that("test : check_trials works", {
  expect_true(check_trials(3))
  expect_length(check_trials(3), 1)
  expect_error(check_trials(-5), "trials should not be negative")
  expect_type(check_trials(5), "logical")
})

test_that("test : check_success works", {
  expect_true(check_success(1:2, 3))
  expect_length(check_success(1:2, 3), 1)
  expect_error(check_success(1:5, 4), "success should not be larger than trials")
  expect_error(check_success(-3, 4), "success should not be negative")
  expect_type(check_success(1:2, 3), "logical")
})
