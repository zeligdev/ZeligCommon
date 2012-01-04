#
#
#
cluster.formula <- function(formula, cluster) {
# mf$formula <- update(mf$formula, paste(". ~ . + ", paste("cluster(",mf$cluster,")")))
# mf$cluster <- NULL
# } else if (mf$robust)
# #"
# mf$formula <- update(formula, paste(". ~ . + ", paste("cluster(1:nrow(",deparse(formula[[2]]),"))")))

  lhs <- deparse(formula[[2]])

  new.formula <- ". ~ . + "
  cluster.part <- "cluster(1:nrow(%s))"
  cluster.part <- sprintf(cluster.part, deparse(formula[[2]]))




  formula
}
