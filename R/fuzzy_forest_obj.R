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
#' @param WGCNA_object output of WGCNA analysis.
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
#' @param ... additional arguments
#' @return data.frame with list of selected features and variable
#'          importance measures.
#' @note This work was partially funded by NSF IIS 1251151.
print.fuzzy_forest <- function(fuzzy_forest, ...) {
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

#' Plot method for fuzzy_forest object.
#' Plots results of fuzzy forest algorithm.
#' @export
#' @param fuzzy_forest A fuzzy_forest object.
#' @param ... aditional arguments
#' @note This work was partially funded by NSF IIS 1251151.

plot.fuzzy_forest <- function(fuzzy_forest, ...) {
  ultimate_survivors = as.numeric(gsub("V([0-9]+)"
                                       , "\\1"
                                       , row.names(fuzzy_forest$final_rf$importance)
  )
  )
  us_modules = rep(NA, length(ultimate_survivors))
  for (i in 1:length(ultimate_survivors)){
    j = ultimate_survivors[i]
    us_modules[i] = fuzzy_forest$module_membership[[j]]
  }
  us_modules = as.data.frame(prop.table(table(us_modules))*100)
  us_modules = cbind(us_modules, rep("us", nrow(us_modules)))
  names(us_modules) = c("module", "percent", "type")
  df = as.data.frame(prop.table(table(fuzzy_forest$module_membership))*100)
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
    labs(list(title = "Module Membership Distribution"
              , x = "Module"
              , y = "Percentage of features in module"
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
