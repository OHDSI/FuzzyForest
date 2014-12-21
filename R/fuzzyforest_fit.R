#' Fits fuzzy forest algorithm.
#'
#' Fits fuzzy forest algorithm.  Returns
#' fuzzy forest object.
#' @export
#' @param X A data.frame with feature vectors.
#' @param y Response vector.
#' @param module_membership A data.frame giving the module membership of each
#'                          feature.
#'                          The first column gives the name of feature.
#'                          The second column gives the name of partition.
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
#' @note This work was partially funded by NSF IIS 1251151.
fuzzyforest <- function(X, y, module_membership, proportion_kept,
                        screening_mtry, selection_mtry) {
  module_membership[, 1] <- as.character(module_membership[, 1])
  module_membership[, 2] <- as.character(module_membership[, 2])
}






