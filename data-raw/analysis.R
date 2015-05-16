#For information about the data set go to the
#the following site: http://labs.genetics.ucla.edu/horvath/CoexpressionNetwork/Rpackages/WGCNA/Tutorials/
#Go to the following part of the page:
#I. Network analysis of liver expression data from female mice: finding modules related to body weight
rm(list=ls()) #will remove ALL objects
library(randomForest)
library(WGCNA)
library(fuzzyforest)
library(flashClust)

# #Read in the female liver data set
# femData = read.csv("LiverFemale3600.csv", stringsAsFactors=FALSE);
# # Take a quick look at what is in the data set:
# dim(femData);
# names(femData);
# datExpr0 = as.data.frame(t(femData[, -c(1:8)]));
# names(datExpr0) = femData$substanceBXH;
# rownames(datExpr0) = names(femData)[-c(1:8)];
# gsg = goodSamplesGenes(datExpr0, verbose = 3);
# gsg$allOK
# if (!gsg$allOK)
# {
#   # Optionally, print the gene and sample names that were removed:
#   if (sum(!gsg$goodGenes)>0)
#     printFlush(paste("Removing genes:", paste(names(datExpr0)[!gsg$goodGenes],
#                                               collapse = ", ")));
#   if (sum(!gsg$goodSamples)>0)
#     printFlush(paste("Removing samples:",
#                      paste(rownames(datExpr0)[!gsg$goodSamples], collapse = ", ")));
#   # Remove the offending genes and samples from the data:
#   datExpr0 = datExpr0[gsg$goodSamples, gsg$goodGenes]
# }
#
# sampleTree <- flashClust(dist(datExpr0), method = "average")
# # Plot the sample tree: Open a graphic output window of size 12 by 9 inches
# # The user should change the dimensions if the window is too large or too small
# sizeGrWindow(12, 9)
# # pdf(file = "Plots/sampleClustering.pdf", width = 12, height = 9)
# par(cex = .6)
# par(mar = c(0,4,2,0))
# plot(sampleTree, main = "Sample clustering to detect outliers", sub = "",
#      xlab = "", cex.lab=1.5, cex.axis = 1.5, cex.main = 2)
# # Plot a line to show the cut
# abline(h = 15, col = "red")
# # Determine cluster under the line
# clust <- cutreeStatic(sampleTree, cutHeight = 15, minSize = 10)
# table(clust)
# # clust 1 contains the samples we want to keep
# keepSamples <- (clust==1)
# datExpr <- datExpr0[keepSamples, ]
# nGenes <- ncol(datExpr)
# nSamples <- nrow(datExpr)
#
# traitData <- read.csv("ClinicalTraits.csv")
# dim(traitData)
# names(traitData)
#
# # remove columns that hold information we do not need.
# allTraits <- traitData[, -c(31, 16)]
# allTraits <- allTraits[, c(2, 11:36)]
# dim(allTraits)
# names(allTraits)
#
# # Form a data frame analogous to expression data that will hold the clinical traits
# femaleSamples <- rownames(datExpr)
# traitRows <- match(femaleSamples, allTraits$Mice)
# datTraits <- allTraits[traitRows, -1]
# rownames(datTraits) <- allTraits[traitRows, 1]
# collectGarbage()
#
# # Re-cluster samples
# sampleTree2 <- flashClust(dist(datExpr), method = "average")
# # Convert traits to a color representation: white means low, red means high,
# # grey means missing entry
# traitColors <- numbers2colors(datTraits, signed = FALSE)
# # Plot the sample dendrogram and the colors underneath
# plotDendroAndColors(sampleTree2, traitColors,
#                     groupLabels = mean(datTraits),
#                     main = "Sample dendrogram and trait heatmap")
#n is 134
#datExpr holds the expression level data, there are 3600
#features in it.
#datTraits contains data on traits
# enableWGCNAThreads()
# # Choose a set of soft-thresholding powers
# powers = c(c(1:10), seq(from = 12, to=20, by=2))
# # Call the network topology analysis function
# sft = pickSoftThreshold(datExpr, powerVector = powers, verbose = 5)
# # Plot the results:
# sizeGrWindow(9, 5)
# par(mfrow = c(1,2));
# cex1 = 0.9;
# # Scale-free topology fit index as a function of the soft-thresholding power
# plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
#      xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
#      main = paste("Scale independence"));
# text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
#      labels=powers,cex=cex1,col="red");
# # this line corresponds to using an R^2 cut-off of h
# abline(h=0.90,col="red")
# # Mean connectivity as a function of the soft-thresholding power
# plot(sft$fitIndices[,1], sft$fitIndices[,5],
#      xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
#      main = paste("Mean connectivity"))
# text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")

# net = blockwiseModules(datExpr, power = 6,
#                        TOMType = "unsigned", minModuleSize = 30,
#                        reassignThreshold = 0, mergeCutHeight = 0.25,
#                        numericLabels = FALSE, pamRespectsDendro = FALSE,
#                        saveTOMs = TRUE,
#                        saveTOMFileBase = "femaleMouseTOM",
#                        verbose = 3)
#
# # open a graphics window
# sizeGrWindow(12, 9)
# # Convert labels to colors for plotting
# mergedColors = labels2colors(net$colors)
# # Plot the dendrogram and the module colors underneath
# plotDendroAndColors(net$dendrograms[[1]], mergedColors[net$blockGenes[[1]]],
#                     "Module colors",
#                     dendroLabels = FALSE, hang = 0.03,
#                     addGuide = TRUE, guideHang = 0.05)
#
# moduleLabels = net$colors
# moduleColors = labels2colors(net$colors)
# MEs = net$MEs;
# geneTree = net$dendrograms[[1]];






#begin fuzzy forest analysis
# net = blockwiseModules(datExpr, power = 6,
#                        TOMType = "unsigned", minModuleSize = 30,
#                        reassignThreshold = 0, mergeCutHeight = 0.25,
#                        numericLabels = TRUE, pamRespectsDendro = FALSE,
#                        saveTOMs = TRUE,
#                        saveTOMFileBase = "femaleMouseTOM",
#                        verbose = 3)

WGCNA_params <- WGCNA_control(p=6, minModuleSize=30, TOMType = "unsigned",
                              reassignThreshold = 0, mergeCutHeight = 0.25,
                              numericLabels = FALSE, pamRespectsDendro = FALSE)
mtry_factor <- 1
drop_fraction <- .5
number_selected <- 30
keep_fraction <- .1
min_ntree <- 2500
ntree_factor <- 1
final_ntree <- 5000
screen_params <- screen_control(drop_fraction=drop_fraction,
                                keep_fraction=keep_fraction,
                                min_ntree=min_ntree, mtry_factor=mtry_factor,
                                ntree_factor=ntree_factor)
select_params <- select_control(drop_fraction=drop_fraction,
                                number_selected=number_selected,
                                min_ntree=min_ntree, mtry_factor=mtry_factor,
                                ntree_factor=ntree_factor)
# y <- datTraits$weight_g
# X <- datExpr
# missing_y <- which(is.na(y))
# X <- X[-missing_y, ]
# y <- y[-missing_y]
#X_impute <- rfImpute(X, y, iter=5, ntree=5000)
#genes with the most missing data, we will check this
#to see if there is some bias.
# MMT00007272 MMT00041987 MMT00057721 MMT00051830 MMT00081133
# 26          24          23          20          19
# MMT00014573 MMT00019310 MMT00002392 MMT00019497 MMT00047418
# 18          18          17          17          16
# MMT00047424 MMT00054175 MMT00063552 MMT00023352 MMT00060205
# 16          16          16          15          15
load("Liver_Expr.RData")
y <- Liver_Expr[, 1]
X <- Liver_Expr[, -1]
n <- dim(X)[1]
p <- dim(X)[2]
test_ind <- sample(1:n, ceiling(n/3), replace=TRUE)
X_test <- X[test_ind, ]
y_test <- y[test_ind]
X_train <- X[-test_ind, ]
y_train <- y[-test_ind]
WGCNA_params <- WGCNA_control(p=6, minModuleSize=30, TOMType = "unsigned",
                              reassignThreshold = 0, mergeCutHeight = 0.25,
                              numericLabels = TRUE, pamRespectsDendro = FALSE)
mtry_factor <- 5; drop_fraction <- .25; number_selected <- 30
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
wff_fit <- wff(X_train, y_train, WGCNA_params=WGCNA_params,
              screen_params=screen_params,
              select_params=select_params,
              final_ntree=final_ntree,
              num_processors=4,
              test_features=X_test,
              test_y=y_test)
mtry_factor=6
rf_fit <- randomForest(X_train, y_train, mtry=mtry_factor*sqrt(p), ntree=25000,
                       importance=TRUE)
rf_imp <- importance(rf_fit, type=1, scale=FALSE)
rf_imp <- rf_imp[order(rf_imp[,1], decreasing=TRUE),,drop=FALSE]
#further comparison of the results
wff_fit
modplot(wff_fit)
rf_top30 <- rf_imp[1:30, ,drop=FALSE]
mods <- wff_fit$module_membership
rf_mods <- mods[match(row.names(rf_top30), mods[,1]),]
table(rf_mods[,2])
#in fuzzy forest, not random forests
match_numbers <- sum(wff_fit$feature_list[,1] %in% row.names(rf_imp)[1:30])
matches <- wff_fit$feature_list[,1] %in% row.names(rf_imp)[1:30]
different_features <- match(wff_fit$feature_list[!matches,1], wff_fit$feature_list[,1])
#in random forest, not random forests
rf_testX <- X_test[, which(row.names(rf_top30) %in% names(X))]
test_rf <- randomForest(rf_testX, y_test, ntree=10000)
print(tail(test_rf$mse))
print(tail(wff_fit$final_rf$test$mse))
print(tail(wff_fit$final_rf$test$mse,1)/tail(test_rf$mse,1))
#summary of test set results
#mtry_factor <- 1, drop_fraction <- .25 number_selected <- 30
#keep_fraction <- .05 min_ntree <- 5000 ntree_factor <- 5
#final_ntree <- 25000: rf=10.89875; ff=8.624734, ratio=0.7913508

# #save(datExpr, datTraits, file = "FemaleLiver-01-dataInput.RData")
# #Choose a set of soft-thresholding powers
# powers = c(c(1:10), seq(from = 12, to=20, by=2))
# # Call the network topology analysis function
# sft = pickSoftThreshold(datExpr, powerVector = powers, verbose = 5)
# # Plot the results:
# sizeGrWindow(9, 5)
# par(mfrow = c(1,2));
# cex1 = 0.9;
# # Scale-free topology fit index as a function of the soft-thresholding power
# plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
#      xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
#      main = paste("Scale independence"));
# text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
#      labels=powers,cex=cex1,col="red");
# # this line corresponds to using an R^2 cut-off of h
# abline(h=0.90,col="red")
# # Mean connectivity as a function of the soft-thresholding power
# plot(sft$fitIndices[,1], sft$fitIndices[,5],
#      xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
#      main = paste("Mean connectivity"))
# text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")
# softPower = 6;
# adjacency = adjacency(datExpr, power = softPower);
# # Turn adjacency into topological overlap
# TOM = TOMsimilarity(adjacency);
# dissTOM = 1-TOM
# # Call the hierarchical clustering function
# geneTree = hclust(as.dist(dissTOM), method = "average");
# # Plot the resulting clustering tree (dendrogram)
# sizeGrWindow(12,9)
# plot(geneTree, xlab="", sub="", main = "Gene clustering on TOM-based dissimilarity",
#      labels = FALSE, hang = 0.04);
# # We like large modules, so we set the minimum module size relatively high:
# minModuleSize = 30;
# # Module identification using dynamic tree cut:
# dynamicMods = cutreeDynamic(dendro = geneTree, distM = dissTOM,
#                             deepSplit = 2, pamRespectsDendro = FALSE,
#                             minClusterSize = minModuleSize);
# table(dynamicMods)
# # Convert numeric lables into colors
# dynamicColors = labels2colors(dynamicMods)
# table(dynamicColors)
# # Plot the dendrogram and colors underneath
# sizeGrWindow(8,6)
# plotDendroAndColors(geneTree, dynamicColors, "Dynamic Tree Cut",
#                     dendroLabels = FALSE, hang = 0.03,
#                     addGuide = TRUE, guideHang = 0.05,
#                     main = "Gene dendrogram and module colors")
# # Calculate eigengenes
# MEList = moduleEigengenes(datExpr, colors = dynamicColors)
# MEs = MEList$eigengenes
# # Calculate dissimilarity of module eigengenes
# MEDiss = 1-cor(MEs);
# # Cluster module eigengenes
# METree = hclust(as.dist(MEDiss), method = "average");
# # Plot the result
# sizeGrWindow(7, 6)
# plot(METree, main = "Clustering of module eigengenes",
#      xlab = "", sub = "")
# MEDissThres = 0.25
# # Plot the cut line into the dendrogram
# abline(h=MEDissThres, col = "red")
# # Call an automatic merging function
# merge = mergeCloseModules(datExpr, dynamicColors, cutHeight = MEDissThres, verbose = 3)
# # The merged module colors
# mergedColors = merge$colors;
# # Eigengenes of the new merged modules:
# mergedMEs = merge$newMEs;
# sizeGrWindow(12, 9)
# #pdf(file = "Plots/geneDendro-3.pdf", wi = 9, he = 6)
# plotDendroAndColors(geneTree, cbind(dynamicColors, mergedColors),
#                     c("Dynamic Tree Cut", "Merged dynamic"),
#                     dendroLabels = FALSE, hang = 0.03,
#                     addGuide = TRUE, guideHang = 0.05)
# # Rename to moduleColors
# moduleColors = mergedColors
# # Construct numerical labels corresponding to the colors
# colorOrder = c("grey", standardColors(50));
# moduleLabels = match(moduleColors, colorOrder)-1;
# MEs = mergedMEs;
# # Define numbers of genes and samples
# nGenes = ncol(datExpr);
# nSamples = nrow(datExpr);
# # Recalculate MEs with color labels
# MEs0 = moduleEigengenes(datExpr, moduleColors)$eigengenes
# MEs = orderMEs(MEs0)
# moduleTraitCor = cor(MEs, datTraits, use = "p");
# moduleTraitPvalue = corPvalueStudent(moduleTraitCor, nSamples);
# sizeGrWindow(10,6)
# # Will display correlations and their p-values
# textMatrix = paste(signif(moduleTraitCor, 2), "\n(",
#                    signif(moduleTraitPvalue, 1), ")", sep = "");
# dim(textMatrix) = dim(moduleTraitCor)
# par(mar = c(6, 8.5, 3, 3));
# # Display the correlation values within a heatmap plot
# labeledHeatmap(Matrix = moduleTraitCor,
#                xLabels = names(datTraits),
#                yLabels = names(MEs),
#                ySymbols = names(MEs),
#                colorLabels = FALSE,
#                colors = blueWhiteRed(50),
#                textMatrix = textMatrix,
#                setStdMargins = FALSE,
#                cex.text = 0.5,
#                zlim = c(-1,1),
#                main = paste("Module-trait relationships"))
# # Define variable weight containing the weight column of datTrait
# weight = as.data.frame(datTraits$weight_g);
# names(weight) = "weight"
# # names (colors) of the modules
# modNames = substring(names(MEs), 3)
# geneModuleMembership = as.data.frame(cor(datExpr, MEs, use = "p"));
# MMPvalue = as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership), nSamples));
# names(geneModuleMembership) = paste("MM", modNames, sep="");
# names(MMPvalue) = paste("p.MM", modNames, sep="");
# geneTraitSignificance = as.data.frame(cor(datExpr, weight, use = "p"));
# GSPvalue = as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nSamples));
# names(geneTraitSignificance) = paste("GS.", names(weight), sep="");
# names(GSPvalue) = paste("p.GS.", names(weight), sep="");
# module = "brown"
# column = match(module, modNames);
# moduleGenes = moduleColors==module;
# sizeGrWindow(7, 7);
# par(mfrow = c(1,1));
# verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
#                    abs(geneTraitSignificance[moduleGenes, 1]),
#                    xlab = paste("Module Membership in", module, "module"),
#                    ylab = "Gene significance for body weight",
#                    main = paste("Module membership vs. gene significance\n"),
#                    cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = module)
# names(datExpr)
# names(datExpr)[moduleColors=="brown"]
# annot = read.csv(file = "GeneAnnotation.csv");
# dim(annot)
# names(annot)
# probes = names(datExpr)
# probes2annot = match(probes, annot$substanceBXH)
# # The following is the number or probes without annotation:
# sum(is.na(probes2annot))
#     # Create the starting data frame
#     geneInfo0 = data.frame(substanceBXH = probes,
#                            geneSymbol = annot$gene_symbol[probes2annot],
#                            LocusLinkID = annot$LocusLinkID[probes2annot],
#                            moduleColor = moduleColors,
#                            geneTraitSignificance,
#                            GSPvalue)
#     # Order modules by their significance for weight
#     modOrder = order(-abs(cor(MEs, weight, use = "p")));
#     # Add module membership information in the chosen order
#     for (mod in 1:ncol(geneModuleMembership))
#     {
#       oldNames = names(geneInfo0)
#       geneInfo0 = data.frame(geneInfo0, geneModuleMembership[, modOrder[mod]],
#                              MMPvalue[, modOrder[mod]]);
#       names(geneInfo0) = c(oldNames, paste("MM.", modNames[modOrder[mod]], sep=""),
#                            paste("p.MM.", modNames[modOrder[mod]], sep=""))
#     }
#     # Order the genes in the geneInfo variable first by module color, then by geneTraitSignificance
#     geneOrder = order(geneInfo0$moduleColor, -abs(geneInfo0$GS.weight));
#     geneInfo = geneInfo0[geneOrder, ]
#     write.csv(geneInfo, file = "geneInfo.csv")
