set.seed(23516)
library(randomForest)
library(WGCNA)
library(fuzzyforest)
library(foreach)
registerDoSEQ()
load("Liver_Expr.RData")
weight <- Liver_Expr[, 1]
expression_levels <- Liver_Expr[, -1]
# Choose a set of soft-thresholding powers
powers = c(c(1:10), seq(from = 12, to=20, by=2))
# Call the network topology analysis function
sft = pickSoftThreshold(expression_levels, powerVector = powers, verbose = 5)
# Plot the results:
sizeGrWindow(9, 5)
par(mfrow = c(1,2));
cex1 = 0.9;
# Scale-free topology fit index as a function of the soft-thresholding power
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
     main = paste("Scale independence"));
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     labels=powers,cex=cex1,col="red");
# this line corresponds to using an R^2 cut-off of h
abline(h=0.90,col="red")
#Mean connectivity as a function of the soft-thresholding power
plot(sft$fitIndices[,1], sft$fitIndices[,5],
     xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
     main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")
net = blockwiseModules(expression_levels, power = 9,
                       TOMType = "unsigned", minModuleSize = 15,
                       reassignThreshold = 0, mergeCutHeight = 0.25,
                       numericLabels = FALSE, pamRespectsDendro = FALSE,
                       verbose = 3)
browser()
module_membership <- net$colors
mtry_factor <- 1; drop_fraction <- .25; number_selected <- 30
keep_fraction <- .1; min_ntree <- 5000; ntree_factor <- 5
final_ntree <- 25000;
screen_params <- screen_control(drop_fraction=drop_fraction,
                                keep_fraction=keep_fraction,
                                min_ntree=min_ntree, mtry_factor=mtry_factor,
                                ntree_factor=ntree_factor)
select_params <- select_control(drop_fraction=drop_fraction,
                                number_selected=number_selected,
                                min_ntree=min_ntree, mtry_factor=mtry_factor,
                                ntree_factor=ntree_factor)
#standard fuzzy forest
ff_fit <- ff(Liver_Expr, weight, module_membership=module_membership,
            screen_params=screen_params, select_params=select_params,
            final_ntree=final_ntree)
#WGCNA fuzzy forest
WGCNA_params <- WGCNA_control(p=8, minModuleSize=30, TOMType = "unsigned",
                              reassignThreshold = 0, mergeCutHeight = 0.25,
                              numericLabels = FALSE, pamRespectsDendro = FALSE)
wff_fit <- wff(Liver_Expr,weight, WGCNA_params=WGCNA_params,
              screen_params=screen_params,
              select_params=select_params,
              final_ntree=final_ntree,
              num_processors=1)
