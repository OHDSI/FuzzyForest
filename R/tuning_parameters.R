#' Set Parameters for Screening Step of
#' Fuzzy Forests
#'
#' Creates \code{screen_control} object for
#' controlling how feature selection
#' will be carried out on each module.
#' @export
#' @param drop_fraction     A number between 0 and 1.  Percentage of features
#'                          dropped at each iteration.
#' @param stop_fraction     A number between 0 and 1. Proportion features
#'                          from each module to retain at screening step.
#' @param mtry_factor       \code{mtry} is set to
#'                          \eqn{\sqrt(p)}*\code{mtry_factor}.
#' @param ntree_factor      A number greater than 1.  \code{ntree} for each
#'                          random is \code{ntree_factor} times the number
#'                          of features.  For each random forest, \code{ntree}
#'                          is set to \code{max}(\code{min_ntree},
#'                          \code{ntree_factor}*\code{p}).
#' @param min_ntree         Minimum number of trees grown in each random forest.
#' @return An object of type screen_control.
#' @note This work was partially funded by NSF IIS 1251151.
screen_control <- function(drop_fraction=.25, stop_fraction=.05,
                           mtry_factor=1, min_ntree=10000, ntree_factor=10) {
  obj <- list()
  obj$drop_fraction <- drop_fraction
  obj$stop_fraction <- stop_fraction
  obj$mtry_factor <- mtry_factor
  obj$min_ntree <- min_ntree
  obj$ntree_factor <- ntree_factor
  class(obj) <- "screen_control"
  return(obj)
}

#' Set Parameters for Selection Step of
#' Fuzzy Forests
#'
#' Creates \code{selection_control} object for
#' controlling how feature selection
#' will be carried out after features from different
#' modules have been combined.
#' @export
#' @param drop_fraction     A number between 0 and 1.  Percentage of features
#'                          dropped at each iteration.
#' @param number_selected   A positive number. Number of features
#'                          that will be selected by fuzzyforests.
#' @param mtry_factor       A number between 0 and 1.  Mtry for each random forest
#'                          is set to \code{p_current*mtry_fraction} where
#'                          \code{p_current} is the current number of features.
#' @param ntree_factor      A number greater than 1.  \code{ntree} for each
#'                          random is \code{ntree_factor} times the number
#'                          of features.  For each random forest, \code{ntree}
#'                          is set to \code{max}(\code{min_ntree},
#'                          \code{ntree_factor}*\code{p}).
#' @param min_ntree         Minimum number of trees grown in each random forest.
#' @return An object of type selection_control.
#' @note This work was partially funded by NSF IIS 1251151.
select_control <- function(drop_fraction=.25, number_selected=5,
                              mtry_factor=1, min_ntree=10000, ntree_factor=10) {
  obj <- list()
  obj$drop_fraction <- drop_fraction
  obj$number_selected <- number_selected
  obj$mtry_factor <- mtry_factor
  obj$min_ntree <- min_ntree
  obj$ntree_factor <- ntree_factor
  class(obj) <- "select_control"
  return(obj)
}


#' Set Parameters for WGCNA Step of
#' Fuzzy Forests
#'
#' Creates \code{WGCNA_control} object for
#' controlling WGCNA will be carried out.
#' @export
#' @param power             Power of adjacency function.
#' @param ...               Other arguments.  See documentation for
#'                          WGCNA.
#' @return An object of type WGCNA_control.
#' @note This work was partially funded by NSF IIS 1251151.
WGCNA_control <- function(power=6, ...) {
  extra_args <- list(...)
  obj <- list()
  obj$power <- power
  obj$extra_args <- extra_args
  class(obj) <- "WGCNA_control"
  return(obj)
}







