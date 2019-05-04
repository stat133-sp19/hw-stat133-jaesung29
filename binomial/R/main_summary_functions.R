#' @title Binomial mean
#' @description computes mean of binomial distribution
#' @param trials (integer)
#' @param prob (real)
#' @return expected number of successes
#' @export
#' @examples
#' bin_mean(trials = 5, prob = 0.5)
#' bin_mean(10, 0.3)
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_mean(trials, prob)
}

#' @title Binomial variance
#' @description computes variance of a binomial distribution
#' @param trials (integer)
#' @param prob (real)
#' @return variance of the expected value
#' @export
#' @examples
#' bin_variance(trials = 5, prob = 0.5)
#' bin_variance(10, 0.3)
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_variance(trials, prob)
}

#' @title Binomial mode
#' @description computes mode of a binomial distribution
#' @param trials (integer)
#' @param prob (real)
#' @return most probable number of successes
#' @export
#' @examples
#' bin_mode(trials = 5, prob = 0.5)
#' bin_mode(10, 0.3)
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_mode(trials, prob)
}

#' @title Binomial skewness
#' @description computes skewness of a binomial distribution
#' @param trials (integer)
#' @param prob (real)
#' @return skewness of binomial distribution
#' @export
#' @examples
#' bin_skewness(trials = 5, prob = 0.5)
#' bin_skewness(10, 0.3)
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_skewness(trials, prob)
}

#' @title Binomial kurtosis
#' @description computes kurtosis(tailedness) of a binomial distribution
#' @param trials (integer)
#' @param prob (real)
#' @return kurtosis(tailedness) of binomial distribution
#' @export
#' @examples
#' bin_kurtosis(trials = 5, prob = 0.5)
#' bin_kurtosis(10, 0.3)
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  aux_kurtosis(trials, prob)
}
