#' Tune mtry in final random forest.
#'
#' Calculates mse over different values of mtry, returns a
#' plot displaying how the mse changes, and returns the
#' value of mtry with minimum mse.
#'
#'
#' @export
#' @param object            An object of type fuzzy_forest.
#' @param step_size         By default, mtry starts out high (equal to the
#'                          number of features selected) and goes down by
#'                          increments of size \code{step_size}.
#'                          selected by fuzzy forests.
#' @param mtry_grid         A grid of values of \code{mtry} to test.
#'                          If \code{mtry_grid} is non-null, the mse will
#'                          be calculated for each specified value.  In
#'                          this case, the argument of \code{step_size} will
#'                          be ignored.
#' @param X                 A data.frame containing the features selected by
#'                          fuzzy forests.  The names of selected features must
#'                          be among the column names of X.  If X is the training
#'                          set, the resulting mse's may be biased.
#' @param y                 The corresponding set of outcomes.
#' @param main              Title of resulting plot.
#' @return An object of type tune_mtry.
#' @note This work was partially funded by NSF IIS 1251151.
tune_mtry <- function(object, X, y, step_size=1, mtry_grid=NULL,
                      main=NULL) {
  if(is.null(main)) {
   main <- "MSE versus mtry"
  }
  X_select <- X[, which(names(X) %in% object$feature_list[, 1])]
  num_select <- dim(X_select)[2]
  if(is.null(mtry_grid)) {
    mtry_grid <- 1:num_select
  }
  ntree_final <- object$final_rf$ntree
  mse <- rep(0, num_select)
  var_exp <- rep(0, num_select)
  for(i in 1:num_select) {
    cmtry <- mtry_grid[i]
    crf <- randomForest(X_select, y, ntree=ntree_final,
                        mtry=cmtry)
    mse[i] <- crf$mse[ntree_final]
    var_exp[i] <- crf$rsq[ntree_final]
  }

  results = as.data.frame(cbind(mtry_grid, mse))

  p_error = ggplot() +
    geom_point(data = results
               , aes(x = mtry_grid
                     , y = mse
                     , colour = "#EE0000"
               )
               , size = 4
    ) +
    geom_line(data = results
              , aes(x = mtry_grid
                    , y = mse)
              , colour = "#EE0000"
              , lwd = 1
    ) +
    labs(list(title = main
              , x = "mtry"
              , y = "Mean of Squared Residual Error"
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
    scale_y_continuous(expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0)) +
    scale_color_identity(name = '', guide = 'legend',labels = c('MSE'))
  plot(p_error)
  best_mse <- matrix(c(mtry_grid[which.min(mse)], min(mse)), nrow=2)
  row.names(best_mse) <- c("mtry", "minimum_mse")
  return(best_mse)
}
