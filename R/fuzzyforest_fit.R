#' Fits fuzzy forest algorithm.
#'
#' Fits fuzzy forest algorithm.  Returns
#' fuzzy forest object.
#' @export
#' @param X A data.frame.  Each column corresponds to a feature vectors.
#' @param y Response vector.
#' @param module_membership A data.frame with number of rows equal to
#'                          \code{ncol(X)}.
#'                          The first column gives the name of module.
#'                          The second column gives the module membership
#'                          of feature.
#' @param proportion_dropped A number between 0 and 1.  The proportion dropped
#'                          at each stage of the iterative random forests.
#' @param proportion_kept A number between 0 and 1. The percent of features
#'                        kept at each iteration of random forest during the
#'                        screening step.
#' @param screening_mtry A number between 0 and 1.  Mtry for each random forest
#'                        is set to \code{p_current*screening_mtry} where
#'                        \code{p_current} is the current number of features.
#' @param selection_mtry A number betwee 0 and 1.  Mtry for final selection
#'                      step is set to \code{num_survivors*selection_mtry}
#'                      where \code{num_survivors} is the number features
#'                      that have survived the initial screening step.
#' @param ntree_factor A number greater than 1.  \code{ntree} for each random
#'                     is \code{ntree_factor} times the number of features.
#' @note This work was partially funded by NSF IIS 1251151.
fuzzyforest <- function(X, y, module_membership,
                        proportion_dropped, proportion_kept,
                        screening_mtry, selection_mtry, ntree_factor=10) {
  module_membership[, 1] <- as.character(module_membership[, 1])
  module_membership[, 2] <- as.character(module_membership[, 2])
  module_list <- unique(module_membership[, 1])
  for (i in 1:length(module_list)) {
    module <- X[, which(module_membership[, 1] == "1")]
    num_features <- ncol(module)
    #TUNING PARAMETER screen_step_mtry
    mtry <- screening_mtry*num_features
    #TUNING PARAMETER ntree_factor
    ntree <- num_features*ntree_factor



  }



  browser()

}






