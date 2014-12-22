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
#' @param screening_mtry    A number between 0 and 1.  Mtry for each random forest
#'                          is set to \code{p_current*screening_mtry} where
#'                          \code{p_current} is the current number of features.
#' @param selection_mtry    A number betwee 0 and 1.  Mtry for final selection
#'                          step is set to \code{num_survivors*selection_mtry}
#'                          where \code{num_survivors} is the number features
#'                          that have survived the initial screening step.
#' @param ntree_factor      A number greater than 1.  \code{ntree} for each random
#'                          is \code{ntree_factor} times the number of features.
#' @param num_processors    Number of processors used to fit random forests.
#' @note This work was partially funded by NSF IIS 1251151.
fuzzyforest <- function(X, y, module_membership,
                        drop_fraction, stop_fraction,
                        screening_mtry, selection_mtry,
                        ntree_factor=10, num_processors=1) {
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
    mtry <- screening_mtry*num_features
    #TUNING PARAMETER ntree_factor
    ntree <- num_features*ntree_factor
    #TUNING PARAMETER stop_fraction
    cutoff = ceiling(num_features * stop_fraction)
    while (num_features > cutoff){
      rf = `%dopar%`(foreach(ntree = rep(ntree/num_processors, num_processors)
                     , .combine = combine, .packages = 'randomForest'),
                     #second argument to '%dopar%'
                     randomForest(module , y, ntree = ntree, mtry = mtry,
                     importance = TRUE, scale = FALSE))
      var_importance <- rf$importance
      quantile <- quantile(var_importance[,1], drop_fraction)
      trimmed_varlist <- var_importance[var_importance[, 1] > quantile, ]
      features <- row.names(trimmed_varlist)
      module <- module[, which(names(module) %in% features)]
      num_features <- length(features)
      mtry <- screening_mtry*num_features
      ntree <- num_features*ntree_factor
    }
    survivors[[i]] <- cbind(features, trimmed_varlist[, 1])
  }
  survivors <- do.call('rbind', survivors)
  survivors <- as.data.frame(survivors, stringsAsFactors = FALSE)
  survivors[, 2] <- as.numeric(survivors[, 2])
  names(survivors) <- c("featureID", "Permutation VIM")
  survivors
}






