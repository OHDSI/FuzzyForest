#' Fits fuzzy forest algorithm.
#'
#' Fits fuzzy forest algorithm.  Returns
#' fuzzy forest object.
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
#' @return A data.frame with the top ranked features.
#' @note This work was partially funded by NSF IIS 1251151.
fuzzyforest <- function(X, y, module_membership,
                        drop_fraction, stop_fraction,
                        mtry_fraction, ntree_factor=10,
                        num_processors=1) {
  module_membership[, 1] <- as.character(module_membership[, 1])
  module_membership[, 2] <- as.character(module_membership[, 2])
  module_list <- unique(module_membership[, 1])
  cl = parallel::makeCluster(num_processors)
  doParallel::registerDoParallel(cl)
  survivors <- vector('list', length(module_list))
  for (i in 1:length(module_list)) {
    module <- X[, which(module_membership[, 1] == module_list[i])]
    num_features <- ncol(module)
    #TUNING PARAMETER screen_step_mtry
    mtry <- mtry_fraction*num_features
    #TUNING PARAMETER ntree_factor
    ntree <- num_features*ntree_factor
    #TUNING PARAMETER stop_fraction
    target = ceiling(num_features * stop_fraction)
    while (num_features >= target){
      rf = `%dopar%`(foreach(ntree = rep(ntree/num_processors, num_processors)
                     , .combine = combine, .packages = 'randomForest'),
                     #second argument to '%dopar%'
                     randomForest(module , y, ntree = ntree, mtry = mtry,
                     importance = TRUE, scale = FALSE))
      var_importance <- rf$importance
      var_importance <- var_importance[order(var_importance[, 1],
                                             decreasing=TRUE), ]
      reduction <- ceiling(num_features*drop_fraction)
      if(num_features - reduction > target) {
          trimmed_varlist <- var_importance[1:(num_features - reduction), ]
          features <- row.names(trimmed_varlist)
          module <- module[, which(names(module) %in% features)]
          num_features <- length(features)
          mtry <- mtry_fraction*num_features
          ntree <- num_features*ntree_factor
        }
      else {
          num_features <- target - 1
          mod_varlist <- var_importance[, 1][1:target]
          features <- row.names(var_importance)[1:target]
          survivors[[i]] <- cbind(features, mod_varlist)
        }
    }
  }
  parallel::stopCluster(cl)
  survivors <- do.call('rbind', survivors)
  survivors <- as.data.frame(survivors, stringsAsFactors = FALSE)
  survivors[, 2] <- as.numeric(survivors[, 2])
  names(survivors) <- c("featureID", "Permutation VIM")
  X_surv <- X[, names(X) %in% survivors[,1]]
  out <- iterative_RF(X_surv, y, stop_fraction,
                      mtry_fraction, ntree_factor,
                      num_processors)
  return(out)
}








