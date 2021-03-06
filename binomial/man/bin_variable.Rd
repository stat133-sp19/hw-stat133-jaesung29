% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main_functions.R
\name{bin_variable}
\alias{bin_variable}
\title{Binomial variable}
\usage{
bin_variable(trials, prob)
}
\arguments{
\item{trials}{(integer)}

\item{prob}{(real)}
}
\value{
a list of trials and probabilities
}
\description{
returns a binomial random variable object
}
\examples{
bin_variable(trials = 5, prob = 0.5)
bin_variable(7, 0.4)
}
