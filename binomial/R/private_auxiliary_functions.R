# Private auxiliary function : Calculate the mean value given the number of trials and probability
aux_mean <- function(trials, prob){
  n <- trials
  p <- prob
  return(n*p)
}

# Private auxiliary function : Calculate the variance given the number of trials and probability
aux_variance <- function(trials, prob){
  n <- trials
  p <- prob
  return(n*p*(1-p))
}

# Private auxiliary function : Calculate the mode given the number of trials and probability
aux_mode <- function(trials, prob){
  n <- trials
  p <- prob
  return(floor(n*p+p))
}

# Private auxiliary function : Calculate the skewness given the number of trials and probability
aux_skewness <- function(trials, prob){
  n <- trials
  p <- prob
  return((1-2*p)/sqrt(n*p*(1-p)))
}

# Private auxiliary function : Calculate the kurtosis given the number of trials and probability
aux_kurtosis <- function(trials, prob){
  n <- trials
  p <- prob
  return((1-6*p*(1-p))/(n*p*(1-p)))
}
