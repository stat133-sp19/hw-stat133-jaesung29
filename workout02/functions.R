# title: "functions.R"
# author: "Jaesung Lee"
# date: "April 15, 2019"

#' @title Future Value Function
#' @description Returns future value given the amount of present value, rate and years elapsed
#' @param amount initial invested amount (numeric)
#' @param rate annual rate of return (numeric)
#' @param years number of years (numeric)
#' @return future value
future_value <- function(amount, rate, years) {
  amount*((1+rate)^(years))
}

#' @title Future Value of Annuity
#' @description Returns future value of annuity given the amount of contribution, rate and years elapsed
#' @param contrib contributed amount (numeric)
#' @param rate annual rate of return (numeric)
#' @param years number of years (numeric)
#' @return future value of annuity
annuity <- function(contrib, rate, years) {
  mul_a = (((1+rate)^years)-1)/rate
  contrib*mul_a
}

#' @title Future Value of Growing Annuity
#' @description Returns future value of growing annuity given the amount of contribution, rate(return and growth) and years elapsed
#' @param contrib contributed amount (numeric)
#' @param rate annual rate of return (numeric)
#' @param growth annual growth rate (numeric)
#' @param years number of years (numeric)
#' @return future value of gorwing annuity
growing_annuity <- function(contrib, rate, growth, years) {
  mul_ga = (((1+rate)^years)-((1+growth)^years))/(rate-growth)
  contrib*mul_ga
}