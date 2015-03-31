#' Fuzzy Forest Object
#'
#' Fuzzy forests returns an object of type
#' fuzzyforest.
#' @export
#' @param feature_list      List of selected features along with variable
#'                          importance measures.
#' @param final_rf          A final random forest fit using the features
#'                          selected by fuzzy forests.
#' @param module_membership Module membership of each feature.
#' @param WGCNA_object      If applicable, output of WGCNA analysis.
#' @param survivor_list     List of features that have survived screening step.
#' @param selection_list    List of features retained at each iteration of
#'                          selection step.
#' @return An object of type fuzzy_forest.
#' @note This work was partially funded by NSF IIS 1251151.
fuzzy_forest <- function(feature_list, final_rf, module_membership,
                         WGCNA_object=NULL, survivor_list, selection_list) {
  out <- list()
  out[[1]] <- feature_list
  out[[2]] <- final_rf
  out[[3]] <- module_membership
  out[[4]] <- module_membership
  out[[5]] <- survivor_list
  out[[6]] <- selection_list
  names(out) <- c("feature_list", "final_rf", "module_membership",
                  "WGCNA_object", "survivor_list", "selection_list")
  class(out) <- "fuzzy_forest"
  return(out)
}


#' Print fuzzy_forest object.
#' Prints output from fuzzy forests algorithm.
#' @export
#' @param x   A fuzzy_forest object.
#' @param ... Additional arguments not in use.
#' @return data.frame with list of selected features and variable
#'          importance measures.
#' @note This work was partially funded by NSF IIS 1251151.
print.fuzzy_forest <- function(x, ...) {
  print(x$feature_list)
}

#' Predict method for fuzzy_forest object.
#' Obtains predictions from fuzzy forest algorithm.
#' @export
#' @param object   A fuzzy_forest object.
#' @param new_data A matrix or data.frame containing new_data.
#'                 Pay close attention to ensure feature names
#'                 match between training set and test set
#'                 data.frame.
#' @param ...      Additional arguments not in use.
#' @return A vector of predictions
#' @note This work was partially funded by NSF IIS 1251151.
predict.fuzzy_forest <- function(object, new_data, ...) {
  out <- predict(fuzzy_forest$final_rf, new_data)
  return(out)
}

#' Plots relative importance of modules.
#'
#' The plot is designed
#' to depict the size of each module and what percentage of selected
#' features fall into each module.  In particular, it is easy to
#' determine which module is over-represented in the group of selected
#' features.
#' @export
#' @param object   A fuzzy_forest object.
#' @param main Title of plot.
#' @param xlab Title for the x axis.
#' @param ylab Title for the y axis.
#' @param module_labels Labels for the modules.  A data.frame
#'                      or character matrix with first column giving
#'                      the current name of module and second column giving
#'                      the assigned name of each module.
#' @param ... Additional arguments currently not in use.
modplot <- function(object, main=NULL, xlab=NULL, ylab=NULL,
                              module_labels=NULL) {
  if(is.null(main)) {
    main <- "Module Membership Distribution"
  }
  if(is.null(xlab)) {
   xlab <- "Module"
  }
  if(is.null(ylab)) {
    ylab <- "Percentage of features in module"
  }
  if(!is.null(module_labels)) {
    old_labels <- object$module_membership[, 2]
    new_labels <- as.factor(old_labels)
    module_labels <- module_labels[order(module_labels[, 1]), ]
    levels(new_labels) <- module_labels[, 2]
    new_labels <- as.character(new_labels)
    object$module_membership[, 2] <- new_labels

    select_mods <- as.factor(object$feature_list[, 3])
    select_key <- module_labels[which(module_labels[, 1] %in% levels(select_mods)), ,drop=FALSE]
    levels(select_mods)[-1] <- select_key[, 2]
    object$feature_list[, 3] <- as.character(select_mods)
  }
  fuzzy_forest <- object
  us_modules <- fuzzy_forest$feature_list$module_membership
  us_modules <- us_modules[us_modules != "."]
  us_modules = as.data.frame(prop.table(table(us_modules))*100)
  us_modules = cbind(us_modules, rep("us", nrow(us_modules)))
  names(us_modules) = c("module", "percent", "type")
  df = as.data.frame(prop.table(table(fuzzy_forest$module_membership[, 2]))*100)
  df = cbind(df, rep("overall", nrow(df)))
  names(df) = c("module", "percent", "type")
  df = rbind(df
             , us_modules
  )
  module=5
  percent=5
  type=5
  p_module_dist = ggplot(df
                         , aes(x = module
                               , y = percent
                               , fill = type)
  ) +
    geom_bar(stat = "identity"
             , position="dodge"
             , colour = "#999999"
    ) +
    labs(list(title = main
              , x = xlab
              , y = ylab
    )) +
    theme(axis.line = element_line(colour = "black")
          , panel.grid.major = element_blank()
          , panel.grid.minor = element_blank()
          , panel.border = element_blank()
          , panel.background = element_blank()
          , axis.text.y = element_text(size=10)
          , axis.text.x = element_text(size=10)
          , axis.title = element_text(size=12, face="bold")
          , plot.title = element_text(size=14, face="bold")
    ) +
    scale_fill_manual(values = c("#CDC9C9", "#95C9FF")
                      , name = "Category"
                      , breaks=c("overall", "us")
                      , labels = c("Overall", "Selected Features")
    ) +
    scale_y_continuous(expand=c(0,0))
    plot(p_module_dist)
}


#' Relabel modules.
#'
#' Lets user easily re-label modules.  Modules are often labeled according to
#' color in WGCNA.  This function allows the user to rename the modules.
#' @export
#' @param object        An object of type fuzzy_forest.
#' @param module_labels Labels for the modules.  A data.frame
#'                      or character matrix with first column giving
#'                      the current name of module and second column giving
#'                      the assigned name of each module.
#' @note This work was partially funded by NSF IIS 1251151.
relabel_modules <- function(object, module_labels) {
  old_labels <- object$module_membership[, 2]
  new_labels <- as.factor(old_labels)
  module_labels <- module_labels[order(module_labels[, 1]), ]
  levels(new_labels) <- module_labels[, 2]
  new_labels <- as.character(new_labels)
  object$module_membership[, 2] <- new_labels
  return(object)
}

