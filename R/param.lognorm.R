#' Extract Samples from a Distribution in Order to Pass Them to the \code{qi}
#' Function
#' (this is primarily a helper function for the lognorm model)
#' @param obj a zelig object
#' @param num an integer specifying the number of simulations to compute
#' @param ... additional parameters
#' @return a list specifying link, link-inverse, random samples, and ancillary
#' parameters
#' @export
param.lognorm <- function(obj, num=1000, ...) {

  # These are the fitted parameters
  coef <- coef(obj)

  # Append the log-scale
  mu <- c(coef, log(obj$result$scale))

  # These are their correlations
  cov <- vcov(obj)

  # Simulate the results
  simulations <- mvrnorm(num, mu, cov)

  # Return
  list(
       coef = as.matrix(simulations[, 1:length(coef)]),
       alpha = as.matrix(simulations[, -(1:length(coef))]),
       linkinv = survreg.distributions[["lognormal"]]$itrans
       )
}
