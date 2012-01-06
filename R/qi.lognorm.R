#' Compute Quantities of Interest for the Zelig Model lognorm
#' @param obj a zelig object
#' @param x a setx object
#' @param x1 an optional setx object
#' @param y ...
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of interest
#' with their simulations
#' @export
qi.lognorm <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  linkinv <- linkinv(param)
  alpha <- alpha(param)
  beta <- coef(param)

  # Compute expected values for "lognorm" regression
  #
  # This function is nested within qi.lognorm for code-clarity and because it
  # will not be used by any other function
  #
  # @param simulations
  # @param x
  # @return a matrix
  compute.ev <- function (simulations, alpha, x) {
    
    if (is.null(x) || is.na(x))
      # If there are missing explanatory variables, ignore them
      return(NA)

    # Compute eta
    # This value must be *inverted* to be restored to the true "observed" value
    eta <- simulations %*% t(x)

    # Apply inverse link function
    theta <- as.matrix(apply(eta, 2, linkinv))

    # Copied from qi.survreg in Zelig v3.5
    ev <- exp(log(theta) + 0.5*(exp(alpha))^2)
    dimnames(ev) <- dimnames(theta)

    # Return
    ev
  }

  # Compute expected values for "lognorm" regression
  #
  # This function is nested within qi.lognorm for code-clarity and because it
  # will not be used by any other function
  #
  # @param simulations
  # @param x
  # @return a matrix
  compute.ev <- function (ev) {
  }

  # Compute expected values for X and X1
  ev1 <- compute.ev(beta, alpha, x)
  ev2 <- compute.ev(beta, alpha, x1)

  #


  list(
       "Expected Value: E(Y|X)" = ev1,
       "Expected Value: E(Y|X1)" = ev2,
       "First Differences: E(Y|X1) - E(Y|X)" = ev2 - ev1
       )
}
