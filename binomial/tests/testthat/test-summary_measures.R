context("summary measures")

test_that("test : check aux_mean works", {
  expect_equal(aux_mean(20, 0.5), 10)
  expect_equal(aux_mean(10, 0.5), 5)
  expect_length(aux_mean(20, 0.5), 1)
})

test_that("test : check aux_variance works", {
  expect_equal(aux_variance(20, 0.5), 5)
  expect_equal(aux_variance(10, 0.5), 2.5)
  expect_length(aux_variance(20, 0.5), 1)
})

test_that("test : check aux_mode works", {
  expect_equal(aux_mode(20, 0.5), 10)
  expect_length(aux_mode(10, 0.5), 5)
  expect_length(aux_mode(20, 0.5), 1)
})

test_that("test : check aux_skewness works", {
  expect_equal(aux_skewness(20,0.5),(1-2*0.5)/sqrt(20*0.5*0.5))
  expect_equal(aux_skewness(10,0.1),(1-2*0.1)/sqrt(10*0.1*0.9))
  expect_length(aux_skewness(10, 0.3), 1)
})

test_that("test : check aux_kurtosis works", {
  expect_equal(aux_kurtosis(20,0.5),(1-6*0.5*0.5)/(20*0.5*0.5))
  expect_equal(aux_kurtosis(10,0.1),(1-6*0.1*0.9)/(10*0.1*0.9))
  expect_length(aux_kurtosis(20, 0.5), 1)
})
