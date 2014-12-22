#' Fits iterative random forest algorithm.
#'
#' Fits iterative random forest algorithm.  Returns
#' data.frame with variable importances and top rated features.
#' @export
#' @param X                 A data.frame.
#'                          Each column corresponds to a feature vectors.
#' @param y                 Response vector.
#' @param module_membership A data.frame with number of rows equal to
#'                          \code{ncol(X)}.
#'                          The first column gives the name of module.
#'                          The second column gives the module membership
#'                          of feature.
#' @param drop_fraction     A number between 0 and 1.  Percentage of features
#'                          dropped at each iteration.
#' @param stop_fraction     A number between 0 and 1. Proportion features
#'                          from each module to retain at screening step.
#' @param mtry_fraction     A number between 0 and 1.  Mtry for each random forest
#'                          is set to \code{p_current*mtry_fraction} where
#'                          \code{p_current} is the current number of features.
#' @param ntree_factor      A number greater than 1.  \code{ntree} for each random
#'                          is \code{ntree_factor} times the number of features.
#' @param num_processors    Number of processors used to fit random forests.
#' @note This work was partially funded by NSF IIS 1251151.
iterative_RF <- function(X, y, drop_fraction, stop_fraction, mtry_fraction,
                         ntree_factor = 10, num_processors = 2) {
  cl = parallel::makeCluster(num_processors)
  doParallel::registerDoParallel(cl)
  num_features <- ncol(X)
  mtry <- mtry_fraction*num_features
  ntree <- num_features*ntree_factor
  target <- ceiling(num_features * stop_fraction)
  current_X <- X
  while (num_features >= target){
    rf = `%dopar%`(foreach(ntree = rep(ntree/num_processors, num_processors)
                           , .combine = combine, .packages = 'randomForest'),
                   #second argument to '%dopar%'
                   randomForest(current_X , y, ntree = ntree, mtry = mtry,
                                importance = TRUE, scale = FALSE))
    var_importance <- rf$importance
    var_importance <- var_importance[order(var_importance[, 1],
                                           decreasing=TRUE), ]
    reduction <- ceiling(num_features*drop_fraction)
    if(num_features - reduction > target) {
      trimmed_varlist <- var_importance[1:(num_features - reduction), ]
      features <- row.names(trimmed_varlist)
      module <- current_X[, which(names(current_X) %in% features)]
      num_features <- length(features)
      mtry <- mtry_fraction*num_features
      ntree <- num_features*ntree_factor
    }
    else {
      num_features <- target - 1
      mod_varlist <- var_importance[, 1][1:target]
      features <- row.names(var_importance)[1:target]
      out <- cbind(features, mod_varlist)
    }
  }
  parallel::stopCluster(cl)
  return(out)
}

