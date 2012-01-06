#' Extract Samples from a Distribution in Order to Pass Them to the \code{qi} Function
#' (this is primarily a helper function for the tobit model)
#' @param obj a zelig object
#' @param num an integer specifying the number of simulations to compute
#' @param ... additional parameters
#' @return a list specifying link, link-inverse, random samples, and ancillary parameters
#' @export
param.tobit <- function(obj, num=1000, ...) {
  cov <- vcov(obj)
  mu <- c(coef(obj), log(obj$result$scale))

  # Return
  list(
       coef = mvrnorm(num, mu=mu, Sigma=cov),
       linkinv = NULL
       )
}
