#' Fits fuzzy forest algorithm.
#'
#' Fits fuzzy forest algorithm.  Returns
#' fuzzy forest object.
#' @export
#' @param X                 A data.frame.
#'                          Each column corresponds to a feature vectors.
#' @param y                 Response vector.  For classification, y should be a
#'                          factor or a character.  For regression, y should be
#'                          numeric.
#' @param module_membership A vector giving module membership of each feature.
#' @param screen_params     Parameters for screening step of fuzzy forests.
#'                          See \code{\link[fuzzyforest]{screen_control}} for details.
#'                          \code{screen_params} is an object of type
#'                          \code{screen_control}.
#' @param select_params     Parameters for selection step of fuzzy forests.
#'                          See \code{\link[fuzzyforest]{select_control}} for details.
#'                          \code{select_params} is an object of type
#'                          \code{select_control}.
#' @param final_ntree       Number trees grown in the final random forest.
#'                          This random forest contains all selected features.
#' @param num_processors    Number of processors used to fit random forests.
#' @param nodesize          Minimum terminal nodesize. 1 if classification.
#'                          5 if regression.  If the sample size is very large,
#'                          the trees will be grown extremely deep.
#'                          This may lead to issues with memory usage and may
#'                          lead to significant increases in the time it takes
#'                          the algorithm to run.  In this case,
#'                          it may be useful to increase \code{nodesize}.
#' @examples
#' n <- 500
#' minCor = .66
#' seed1 <- rnorm(n)
#' seed2 <- rnorm(n)
#' p <- 100
#' X1 <- WGCNA::simulateModule(seed1, nGenes=p/2, minCor=.66, maxCor=.99,
#'                      propNegativeCor=.01)
#' X2 <- WGCNA::simulateModule(seed2, nGenes=p/2, minCor=.66, maxCor=.99,
#'                      propNegativeCor=.01)
#' beta1 <- c(c(5, 2, 1, 0, 0), rep(0, p/2-5))
#' beta2 <- beta1
#' X <- cbind(X1, X2)
#' beta <- c(beta1, beta2)
#' y <- X%*%beta + rnorm(n)
#' X <- as.data.frame(X)
#' names(X) <- paste("V",1:p,sep="")
#' module_membership <- as.character(rep(1:2,each=p/2))
#' fit <- ff(X, y, module_membership,
#'                   screen_params=screen_control(min_ntree=100),
#'                   select_params=select_control(number_selected=3, min_ntree=100))
#' @return A data.frame with the top ranked features.
#' @note This work was partially funded by NSF IIS 1251151.
ff <- function(X, y, module_membership,
                        screen_params = screen_control(min_ntree=5000),
                        select_params = select_control(min_ntree=5000),
                        final_ntree = 500,
                        num_processors=1, nodesize) {
  CLASSIFICATION <- is.factor(y)
  screen_control <- screen_params
  select_control <-  select_params
  module_list <- unique(module_membership)
  cl = parallel::makeCluster(num_processors)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))
  survivors <- vector('list', length(module_list))
  drop_fraction <- screen_control$drop_fraction
  mtry_factor <- screen_control$mtry_factor
  ntree_factor <- screen_control$ntree_factor
  min_ntree <- screen_control$min_ntree
  keep_fraction <- screen_control$keep_fraction
  if(ncol(X)*keep_fraction < select_control$number_selected){
    warning(c("ncol(X)*keep_fraction < number_selected", "\n",
              "number_selected will be set to floor(ncol(X)*keep_fraction)"))
              select_control$number_selected <- floor(ncol(X)*keep_fraction)
  }

  for (i in 1:length(module_list)) {
    module <- X[, which(module_membership == module_list[i])]
    num_features <- ncol(module)
    #TUNING PARAMETER mtry_factor
    if(CLASSIFICATION == TRUE) {
      mtry <- ceiling(mtry_factor*num_features/3)
      if(missing(nodesize)){
        nodesize <- 1
      }
    }
    if(CLASSIFICATION == FALSE) {
      mtry <- ceiling(mtry_factor*sqrt(num_features))
      if(missing(nodesize)){
        nodesize <- 5
      }
    }
    #TUNING PARAMETER ntree_factor
    ntree <- max(num_features*ntree_factor, min_ntree)
    #TUNING PARAMETER keep_fraction
    target = ceiling(num_features * keep_fraction)
    while (num_features >= target){
      rf = `%dopar%`(foreach(ntree = rep(ntree/num_processors, num_processors)
                     , .combine = combine, .packages = 'randomForest'),
                     #second argument to '%dopar%'
                     randomForest(module , y, ntree = ntree, mtry = mtry,
                     importance = TRUE, scale = FALSE, nodesize=nodesize))
      var_importance <- importance(rf, type=1, scale=FALSE)
      var_importance <- var_importance[order(var_importance[, 1],
                                             decreasing=TRUE), ,drop=FALSE]
      reduction <- ceiling(num_features*drop_fraction)
      if(num_features - reduction > target) {
          trimmed_varlist <- var_importance[1:(num_features - reduction), ,drop=FALSE]
          features <- row.names(trimmed_varlist)
          module <- module[, which(names(module) %in% features)]
          num_features <- length(features)
          if(CLASSIFICATION == TRUE) {
            mtry <- ceiling(mtry_factor*num_features/3)
          }
          if(CLASSIFICATION == FALSE) {
            mtry <- ceiling(mtry_factor*sqrt(num_features))
          }
          ntree <- max(num_features*ntree_factor, min_ntree)
        }
      else {
          num_features <- target - 1
          mod_varlist <- var_importance[, 1][1:target]
          features <- row.names(var_importance)[1:target]
          survivors[[i]] <- cbind(features, mod_varlist)
          row.names(survivors[[i]]) <- NULL
          survivors[[i]] <- as.data.frame(survivors[[i]])
          survivors[[i]][, 1] <- as.character(survivors[[i]][, 1])
          survivors[[i]][, 2] <- as.numeric(as.character(survivors[[i]][, 2]))
        }
    }
  }
  survivor_list <- survivors
  names(survivor_list) <- module_list
  survivors <- do.call('rbind', survivors)
  survivors <- as.data.frame(survivors, stringsAsFactors = FALSE)
  survivors[, 2] <- as.numeric(survivors[, 2])
  names(survivors) <- c("featureID", "Permutation VIM")
  X_surv <- X[, names(X) %in% survivors[,1]]
  select_args <- list(X_surv, y, num_processors, nodesize)
  select_args <- c(select_args, select_control)
  names(select_args)[1:4] <- c("X", "y", "num_processors", "nodesize")
  select_results <- do.call("select_RF", select_args)
  final_list <- select_results[[1]]
  selection_list <- select_results[[2]]
  final_list[, 2] <- round(as.numeric(final_list[, 2]), 4)
  row.names(final_list) <- NULL
  colnames(final_list) <- c("feature_name", "variable_importance")
  final_list <- as.data.frame(final_list, stringsAsFactors=FALSE)
  final_list[, 2] <- as.numeric(final_list[, 2])
  select_mods <- module_membership[which(names(X) %in% final_list[,1])]
  final_list <- cbind(final_list, select_mods,stringsAsFactors=FALSE)
  names(final_list)[3] <- "module_membership"
  final_X <- X[, names(X) %in% final_list[, 1],drop=FALSE]
  if(CLASSIFICATION == TRUE) {
    final_mtry <- ceiling(select_control$mtry_factor*ncol(final_list)/3)
  }
  if(CLASSIFICATION == FALSE) {
    final_mtry <- ceiling(select_control$mtry_factor*sqrt(ncol(final_list)))
  }
  final_rf <- randomForest(x=final_X, y=y, mtry=final_mtry, ntree=final_ntree,
                           importance=TRUE, nodesize=nodesize)
  module_membership <- as.data.frame(cbind(names(X), module_membership))
  names(module_membership) <- c("feature_name", "module")
  out <- fuzzy_forest(final_list, final_rf, module_membership,
                      survivor_list=survivor_list, selection_list=selection_list)

  return(out)
}


#' Fits WGCNA based fuzzy forest algorithm.
#'
#' Fits fuzzy forest algorithm using WGCNA.  Returns
#' fuzzy forest object.
#' @export
#' @param X                 A data.frame.
#'                          Each column corresponds to a feature vectors.
#' @param y                 Response vector.  For classification, y should be a
#'                          factor or a character.  For regression, y should be
#'                          numeric.
#' @param WGCNA_params      Parameters for WGCNA.
#'                          See \code{\link[WGCNA]{blockwiseModules}} and
#'                          \code{\link[fuzzyforest]{WGCNA_control}} for details.
#'                          \code{WGCNA_params} is an object of type
#'                          \code{WGCNA_control}.
#' @param screen_params     Parameters for screening step of fuzzy forests.
#'                          See \code{\link[fuzzyforest]{screen_control}} for details.
#'                          \code{screen_params} is an object of type
#'                          \code{screen_control}.
#' @param select_params     Parameters for selection step of fuzzy forests.
#'                          See \code{\link[fuzzyforest]{select_control}} for details.
#'                          \code{select_params} is an object of type
#'                          \code{select_control}.
#' @param final_ntree       Number trees grown in the final random forest.
#'                          This random forest contains all selected features.
#' @param nodesize          Minimum terminal nodesize. 1 if classification.
#'                          5 if regression.  If the sample size is very large,
#'                          the trees will be grown extremely deep.
#'                          This may lead to issues with memory usage and may
#'                          lead to significant increases in the time it takes
#'                          the algorithm to run.  In this case,
#'                          it may be useful to increase \code{nodesize}.
#' @return A data.frame with the top ranked features.
#' @note This work was partially funded by NSF IIS 1251151.
wff <- function(X, y, WGCNA_params=WGCNA_control(p=6),
                        screen_params=screen_control(min_ntree=5000),
                        select_params=select_control(min_ntree=5000),
                        final_ntree=500, num_processors=1, nodesize) {
  #browser()
  CLASSIFICATION <- is.factor(y)
  if(CLASSIFICATION == TRUE) {
    if(missing(nodesize)){
      nodesize <- 1
    }
  }
  if(CLASSIFICATION == FALSE) {
    if(missing(nodesize)){
      nodesize <- 5
    }
  }
  WGCNA_control <- WGCNA_params
  screen_control <- screen_params
  select_control <-  select_params
  WGCNA_args <- list(X,WGCNA_control$power)
  WGCNA_args <- c(WGCNA_args, WGCNA_control$extra_args)
  names(WGCNA_args) <- c("datExpr", "power", names(WGCNA_control$extra_args))
  bwise <- do.call("blockwiseModules", WGCNA_args)
  module_membership <- bwise$colors
  screen_drop_fraction <- screen_control$drop_fraction
  screen_keep_fraction <- screen_control$keep_fraction
  screen_mtry_factor <- screen_control$mtry_factor
  screen_ntree_factor <- screen_control$ntree_factor
  screen_min_ntree <- screen_control$min_ntree
  out <- ff(X, y, module_membership,
                    screen_control, select_control, final_ntree,
                    num_processors, nodesize=nodesize)
  out$WGCNA_object <- bwise
  return(out)
}






