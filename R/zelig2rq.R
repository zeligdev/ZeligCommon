#' Interface between the Zelig Model rq and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2rq <- function (formula, ..., weights = NULL, tau = .5, data) {

  if (is.character(weights))
    weights <- as.name(weights)

  list(
       .function = "rq",
       formula = formula,
       data = data,
       ...
       )
}
