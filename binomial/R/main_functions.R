#' @title Binomial choose
#' @description computes number of combinations of successes
#' @param n trials (integer)
#' @param k successes (integer)
#' @return number of combinations of successes
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
bin_choose <- function(n, k){
  if(any(k > n)) {
    stop("k cannot be greater than n")
  }
  return(factorial(n)/(factorial(k)*factorial(n-k)))
}


#' @title Binomial probability
#' @description computes probability of successes
#' @param success (integer)
#' @param trials (integer)
#' @param prob (real)
#' @return probability of successes in trials
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' bin_probability(0:2, 5, 0.5)
#' bin_probability(55, 100, 0.45)
bin_probability <- function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  k <- success
  n <- trials
  p <- prob
  c <- bin_choose(n, k)
  return(c*(p^k)*(1-p)^(n-k))
}


#' @title Binomial distribution
#' @description computes binomial probability distribution
#' @param trials (integer)
#' @param prob (real)
#' @return data frame with binomial probability distribution
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
#' bin_distribution(7, 0.4)
bin_distribution <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  successes <- 0:trials
  probdist <- bin_probability(successes, trials, prob)
  df <- data.frame("success" = successes, "probability" = probdist)
  class(df) <- c("bindis","data.frame")
  return(df)
}

#' @export
plot.bindis <- function(x, ...) {
  barplot(x$probability, names.arg = x$success, main = "Binomial distribution", xlab = "successes", ylab = "probability")
  }


#' @title Binomial cumulative
#' @description computes cumulative probabilities and binomial distribution
#' @param trials (integer)
#' @param prob (real)
#' @return data frame with cumulative probabilities
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
#' bin_cumulative(7, 0.4)
bin_cumulative <- function(trials, prob) {
  x <- bin_distribution(trials, prob)
  x$cumulative = cumsum(x$probability)
  class(x) <- c("bincum", "data.frame")
  return(x)
}

#' @export
plot.bincum <- function(x, ...) {
  plot(x = x$success, y = x$cumulative, type = "o", main = "Binomial cumulative", xlab = "successes", ylab = "probability")
}


#' @title Binomial variable
#' @description returns a binomial random variable object
#' @param trials (integer)
#' @param prob (real)
#' @return a list of trials and probabilities
#' @export
#' @examples
#' bin_variable(trials = 5, prob = 0.5)
#' bin_variable(7, 0.4)
bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  ret <- list(trials, prob)
  class(ret) <- c("binvar")
  return(ret)
}

#' @export
print.binvar <- function(x, ...) {
  cat("'Binomial Variable'\n\n")
  cat("Parameters\n")
  cat("- number of trials:", x[[1]], "\n")
  cat("- prob of success :", x[[2]], "\n")
}


#' @export
summary.binvar <- function(x, ...) {
  trials <- x[[1]]
  prob <- x[[2]]
  mean <- aux_mean(x[[1]], x[[2]])
  variance <- aux_variance(x[[1]], x[[2]])
  mode <- aux_mode(x[[1]], x[[2]])
  skewness <- aux_skewness(x[[1]], x[[2]])
  kurtosis <- aux_kurtosis(x[[1]], x[[2]])
  list <- list(trials, prob, mean, variance, mode, skewness, kurtosis)
  class(list) <- c("summary.binvar")
  return(list)
}


#' @export
print.summary.binvar <- function(x, ...) {
  strings <- c('"Summary Binomial"',
               "",
               "Parameters",
               paste("- number of trials:", x[[1]]),
               paste("- prob of success :", x[[2]]),
               "",
               "Measures",
               paste("- mean    :", x[[3]]),
               paste("- variance:", x[[4]]),
               paste("- mode    :", x[[5]]),
               paste("- skewness:", x[[6]]),
               paste("- kurtosis:", x[[7]]))
  for (s in strings) {
    print(noquote(s))
  }
}
