#' Fuzzy Forest Object
#'
#' Fuzzy forests returns an object of type
#' fuzzyforest.
#' @export
#' @param feature_list      List of selected features with variable
#'                          importance.
#' @param final_rf          The final random forest that was fit by
#'                          fuzzy forests.
#' @param module_membership Module membership of each feature.
#' @return An object of type fuzzy_forest.
#' @note This work was partially funded by NSF IIS 1251151.
fuzzy_forest <- function(feature_list, final_rf, module_membership,
                         WGCNA_object=NULL) {
  out <- list()
  out[[1]] <- feature_list
  out[[2]] <- final_rf
  out[[3]] <- module_membership
  out[[4]] <- module_membership
  names(out) <- c("feature_list", "final_rf", "module_membership", "WGCNA_object")
  class(out) <- "fuzzy_forest"
  return(out)
}


#' Print fuzzy_forest object.
#' Prints output from fuzzy forests algorithm.
#' @export
#' @param fuzzy_forest A fuzzy_forest object.
#' @return data.frame with list of selected features and variable
#'          importance measures.
#' @note This work was partially funded by NSF IIS 1251151.
print.fuzzy_forest <- function(fuzzy_forest) {
  return(fuzzy_forest$feature_list)
}

#' Predict method for fuzzy_forest object.
#' Obtains predictions from fuzzy forest algorithm.
#' @export
#' @param fuzzy_forest A fuzzy_forest object.
#' @param new_data A matrix or data.frame containing new_data.
#'                 Pay close attention to ensure feature names
#'                 match between training set and test set
#'                 data.frame.
#'
#' @return A vector of predictions
#' @note This work was partially funded by NSF IIS 1251151.
predict.fuzzy_forest <- function(fuzzy_forest, new_data) {
  out <- predict(fuzzy_forest$final_rf, new_data)
  return(out)
}



