#' Interface between the Zelig Model lognorm and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2lognorm <- function (formula, ..., robust = FALSE, cluster = NULL, data) {

  if (!(is.null(cluster) || robust))
    stop("If cluster is specified, then `robust` must be TRUE")

  # Add cluster term
  if (robust || !is.null(cluster))
    formula <- cluster.formula(formula, cluster)

  # Return
  list(
       .function = "survreg",

       formula = formula,
       dist = "lognormal",
       robust = robust,
       data = data,
       ...
       )
}



# cluster.formula <- function (formula, cluster) {
#   if (!(is.null(cluster) || robust))
#     stop("If cluster is specified, then `robust` must be TRUE")
# 
# 
#   if (!is.null(cluster)) {
#     # If cluster is specified explicitly
#     end <- paste("cluster(", cluster, ")", sep="")
#     end <- paste(". ~ . +", end)
#     formula <- update(formula, end)
#   }
# 
#   else if (robust) {
#     # Otherwise we parse the formula's response variable
#     end <- deparse(formula[[2]])
#     end <- paste("cluster(1:nrow(", end, "))")
#     end <- paste(". ~ . +", end)
#     formula <- update(formula, end)
#   }
# 
#   formula
# }
