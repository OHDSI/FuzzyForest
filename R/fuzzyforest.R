#' fuzzyforest: an implementation of the fuzzy forest algorithm in R.
#'
#' This package implements fuzzy forests and integrates the fuzzy
#' forests algorithm with the package, \pkg{WGCNA}.
#'
#'
#' @docType package
#' @import randomForest
#' @import foreach
#' @import doParallel
#' @import ggplot2
#' @name fuzzyforest
NULL

#' Liver Expression Data from Female Mice
#'
#' A data set containing gene expression levels in liver tissue from female
#' mice. This data set is a subset of the liver expression data set
#' from the WGCNA tutorial \url{http://labs.genetics.ucla.edu/horvath/CoexpressionNetwork/Rpackages/WGCNA/Tutorials/}.
#' The tutorial contains further information about the data set as well as
#' extensive examples of WGCNA.
#'
#' \itemize{
#'   \item The first column contains weight (g) for the 66 mice.
#'   \item The other 3600 columns contain the liver expression levels.
#'  }
#' @docType data
#' @keywords datasets
#' @name Liver_Expr
#' @usage data(Liver_Expr)
#' @format A data frame with 66 rows and 3601
NULL
