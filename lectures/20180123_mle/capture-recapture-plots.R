###################################
# R Program for Capture-Recapture #
###################################

library(tidyverse)

#' Calculate the likelihood for range of values of the parameter, N
#' 
#' @param c Total number of tagged fish (size of first sample)
#' @param n Total number of fish in second sample
#' @param x Number of tagged fish in 2nd sample
#' 
#' @return a data frame with two columns: N is a possible value for the
#'   the population size and lik is the likelihood
calc_capture_lik <- function(group_id, c, n, x, max_N = 500) {
  N <- seq(from = 0, to = max_N)
  return(
    data.frame(
      N = N,
      lik = dhyper(x, c, N-c, n),
      group_id = group_id
    )
  )
}

# Read in class results and calculate likelihood at a grid of values for N
results <- read_csv("http://www.evanlray.com/stat343_s2018/lectures/20180123/capture_recapture_results.csv")

lik_combined <- map(results, calc_capture_lik)

#Plot likelihood functions for each group and draw vertical line at maximum of each likelihood function
