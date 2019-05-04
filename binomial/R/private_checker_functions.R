# Private checker function : Check probability if prob is a valid number between 0 and 1
check_prob <- function(prob) {
  if (is.numeric(prob) == TRUE){
    if (0 <= prob & prob <= 1) {
      return(TRUE)
    }
  }
  else {
    stop("invalid prob value")
  }
}

# Private checker function : Check trials if input n is a non-negative integer
check_trials <- function(trials) {
  if (is.numeric(trials) == TRUE){
    if (trials >= 0) {
      return(TRUE)
    }
  }
  else {
    stop("invalid trials value")
  }
}

# Private checker function : Check sucess if an input success is a valid value for number of successes
check_success <- function(success,trials){
  if (all((success >= 0) & (success <= trials) & (success%%1 == 0))){
    return(TRUE)
  }
  else if(any(success > trials)) {
    stop("success cannot be greater than trials")
  }
  else{
    stop("invalid success value")
  }
}
