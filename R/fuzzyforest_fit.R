#' Fits fuzzy forest algorithm.
#'
#' Fits fuzzy forest algorithm.  Returns
#' fuzzy forest object.
#' @export
#' @param X                 A data.frame.
#'                          Each column corresponds to a feature vectors.
#' @param y                 Response vector.
#' @param module_membership A vector giving module membership of each feature.
#' @param number_selected   Number of features selected by fuzzyforest.
#' @param screen_control    A screen control object.
#' @param select_control A selection control object
#' @param num_processors    Number of processors used to fit random forests.
#' @return A data.frame with the top ranked features.
#' @note This work was partially funded by NSF IIS 1251151.
fuzzyforest <- function(X, y, module_membership, number_selected=5,
                        screen_control=screen_control(),
                        select_control=select_control(),
                        num_processors=1) {
  module_list <- unique(module_membership)
  cl = parallel::makeCluster(num_processors)
  doParallel::registerDoParallel(cl)
  survivors <- vector('list', length(module_list))
  drop_fraction <- screen_control$drop_fraction
  mtry_factor <- screen_control$mtry_factor
  ntree_factor <- screen_control$ntree_factor
  min_ntree <- screen_control$min_ntree
  stop_fraction <- screen_control$stop_fraction
  for (i in 1:length(module_list)) {
    module <- X[, which(module_membership == module_list[i])]
    num_features <- ncol(module)
    #TUNING PARAMETER mtry_factor
    mtry <- ceiling(mtry_factor*sqrt(num_features))
    #TUNING PARAMETER ntree_factor
    ntree <- max(num_features*ntree_factor, min_ntree)
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
          mtry <- mtry_factor*sqrt(num_features)
          ntree <- max(num_features*ntree_factor, min_ntree)
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
  select_args <- list(X, y)
  select_args <- c(select_args, select_control)
  out <- do.call("select_RF", select_args)
  return(out)
}


#' Fits WGCNA based fuzzy forest algorithm.
#'
#' Fits fuzzy forest algorithm using WGCNA.  Returns
#' fuzzy forest object.
#' @export
#' @param X                 A data.frame.
#'                          Each column corresponds to a feature vectors.
#' @param y                 Response vector.
#' @param WGCNA_control     An object of class WGCNA_control.
#' @param number_selected   Number of features selected by algorithm.
#' @param screen_control    An object of class screen_control.
#' @param select_control    An object of class selection_control.
#' @param num_processors    Number of processors.
#' @return A data.frame with the top ranked features.
#' @note This work was partially funded by NSF IIS 1251151.
WGCNA_fuzzyforest <- function(X, y, number_selected,
                        WGCNA_control=WGCNA_control(),
                        screen_control=screen_control(),
                        select_control=select_control(), num_processors=1) {

  WGCNA_args <- list(X,WGCNA_control$power)
  WGCNA_args <- c(WGCNA_args, WGCNA_control$extra_args)
  names(WGCNA_args) <- c("datExpr", "power", names(WGCNA_control$extra_args))
  bwise <- do.call("blockwiseModules", WGCNA_args)
  module_membership <- bwise$colors
  screen_drop_fraction <- screen_control$drop_fraction
  screen_stop_fraction <- screen_control$stop_fraction
  screen_mtry_factor <- screen_control$mtry_factor
  screen_ntree_factor <- screen_control$ntree_factor
  screen_min_ntree <- screen_control$min_ntree
  out <- fuzzyforest(X, y, module_membership, number_selected,
                    screen_control, select_control, num_processors)
  return(out)
}






