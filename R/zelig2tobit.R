#' Interface between the Zelig Model tobit and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2tobit <- function (
                         formula, ..., 
                         below = 0, above = Inf, 
                         robust = FALSE,
                         cluster = NULL,
                         data
                         ) {


  #
  formula <- cluster.formula(formula, cluster)

  # 
  formula <- make.surv(formula, below, above)


  list(
       .function = "survreg",
       formula = formula,
       dist = "gaussian",
       data = data,
       robust = robust,
       ...
       )
}


#
make.surv <- function (formula, below, above) {

  lhs <- formula[[2]]

  if (!grepl("Surv", as.character(lhs)))
    return(formula)

  if (!(is.numeric(below) && is.numeric(above))) {
    warning("`below` and `above` must be numeric; ",
            "returning the original formula")
    return(formula)
  }

  if (above == Inf)
    tt <- "left"

  else if (below == -Inf && above == Inf)
    stop("This model does not support censoring. Try the \"normal\" model")

  else if (below == -Inf && above != Inf)
    stop("This model does not support right-censored data")

  else if (is.finite(below) && is.finite(above))
    stop("This model does not support interval-censored data")

  # That is, this model only supports left-censored data

  lhs <- call("Surv", lhs, call("<", below, lhs), type="left")

  formula[[2]] <- lhs

  # Return
  formula
}
