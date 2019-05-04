# Private checker function : Check probability if prob is a valid number between 0 and 1
check_prob <- function(prob) {
  if (!is.numeric(prob)) {
    stop("prob is not a number")
    }
  if (length(prob) != 1) {
    stop("prob does not have length 1")
    }
  if (prob < 0 | prob > 1) {
    stop("prob must be a number betwen 0 and 1")
    }
  return (TRUE)
  }

# Private checker function : Check trials if input n is a non-negative integer
check_trials <-function(trials) {
  if (!is.numeric(trials)) {
    stop("trials is not a number")
    }
  if (!trials%%1==0) {
    stop("trials should be an integer")
    }
  if (length(trials) != 1) {
    stop("trials does not have length 1")
    }
  if (trials < 0) {
    stop("trials should not be negative")
    }
  return(TRUE)
  }

# Private checker function : Check sucess if an input success is a valid value for number of successes
check_success <- function(success, trials) {
  if (!is.numeric(success)) {
    stop("success is not a vector of numbers")
    }
  if (!(all(success >= 0))){
    stop("success should not be negative")
    }
  if (!(all(success <= trials))){
    stop("success should not be larger than trials")
    }
  return(TRUE)
  }
