context("test checker functions")

test_that("test : check_prob works", {
  expect_true(check_prob(0.7))
  expect_true(check_prob(0))
  expect_length(check_prob(0.7), 1)
  expect_error(check_prob(100), "invalid prob value")
  expect_error(check_prob(-1), "invalid prob value")
  expect_type(check_prob(0.5), "logical")
})

test_that("test : check_trials works", {
  expect_true(check_trials(3))
  expect_length(check_trials(3), 1)
  expect_error(check_trials(-5),"invalid trials value")
  expect_type(check_trials(5), "logical")
})

test_that("test : check_success works", {
  expect_true(check_success(1:2, 3))
  expect_length(check_success(1:2, 3), 1)
  expect_error(check_success(1:5, 4), "success cannot be greater than trials")
  expect_error(check_success(-3, 4), "invalid success value")
  expect_type(check_success(1:2, 3), "logical")
})
