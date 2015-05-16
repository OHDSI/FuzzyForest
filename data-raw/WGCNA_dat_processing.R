#This file is designed to demonstrate how the data set used
#in the vignette was created from the raw data set
#For information about the data set go to the
#the following site: http://labs.genetics.ucla.edu/horvath/CoexpressionNetwork/Rpackages/WGCNA/Tutorials/
#Go to the following part of the page:
#I. Network analysis of liver expression data from female mice: finding modules related to body weight
set.seed(323516)
rm(list=ls()) #will remove ALL objects
library(randomForest)
library(WGCNA)
library(fuzzyforest)
library(flashClust)

#Read in the female liver data set
femData = read.csv("LiverFemale3600.csv", stringsAsFactors=FALSE);
# Take a quick look at what is in the data set:
dim(femData);
names(femData);
datExpr0 = as.data.frame(t(femData[, -c(1:8)]));
names(datExpr0) = femData$substanceBXH;
rownames(datExpr0) = names(femData)[-c(1:8)];
gsg = goodSamplesGenes(datExpr0, verbose = 3);
gsg$allOK
if (!gsg$allOK)
{
  # Optionally, print the gene and sample names that were removed:
  if (sum(!gsg$goodGenes)>0)
    printFlush(paste("Removing genes:", paste(names(datExpr0)[!gsg$goodGenes],
                                              collapse = ", ")));
  if (sum(!gsg$goodSamples)>0)
    printFlush(paste("Removing samples:",
                     paste(rownames(datExpr0)[!gsg$goodSamples], collapse = ", ")));
  # Remove the offending genes and samples from the data:
  datExpr0 = datExpr0[gsg$goodSamples, gsg$goodGenes]
}

sampleTree <- flashClust(dist(datExpr0), method = "average")
# Plot the sample tree: Open a graphic output window of size 12 by 9 inches
# The user should change the dimensions if the window is too large or too small
sizeGrWindow(12, 9)
# pdf(file = "Plots/sampleClustering.pdf", width = 12, height = 9)
par(cex = .6)
par(mar = c(0,4,2,0))
plot(sampleTree, main = "Sample clustering to detect outliers", sub = "",
     xlab = "", cex.lab=1.5, cex.axis = 1.5, cex.main = 2)
# Plot a line to show the cut
abline(h = 15, col = "red")
# Determine cluster under the line
clust <- cutreeStatic(sampleTree, cutHeight = 15, minSize = 10)
table(clust)
# clust 1 contains the samples we want to keep
keepSamples <- (clust==1)
datExpr <- datExpr0[keepSamples, ]
nGenes <- ncol(datExpr)
nSamples <- nrow(datExpr)

traitData <- read.csv("ClinicalTraits.csv")
dim(traitData)
names(traitData)

# remove columns that hold information we do not need.
allTraits <- traitData[, -c(31, 16)]
allTraits <- allTraits[, c(2, 11:36)]
dim(allTraits)
names(allTraits)

# Form a data frame analogous to expression data that will hold the clinical traits
femaleSamples <- rownames(datExpr)
traitRows <- match(femaleSamples, allTraits$Mice)
datTraits <- allTraits[traitRows, -1]
rownames(datTraits) <- allTraits[traitRows, 1]
collectGarbage()

# Re-cluster samples
sampleTree2 <- flashClust(dist(datExpr), method = "average")
# Convert traits to a color representation: white means low, red means high,
# grey means missing entry
traitColors <- numbers2colors(datTraits, signed = FALSE)
# Plot the sample dendrogram and the colors underneath
plotDendroAndColors(sampleTree2, traitColors,
                    groupLabels = mean(datTraits),
                    main = "Sample dendrogram and trait heatmap")
#n is 134
#datExpr holds the expression level data, there are 3600
#features in it.
#datTraits contains data on traits
enableWGCNAThreads()
# Choose a set of soft-thresholding powers
powers = c(c(1:10), seq(from = 12, to=20, by=2))
# Call the network topology analysis function
sft = pickSoftThreshold(datExpr, powerVector = powers, verbose = 5)
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
# Mean connectivity as a function of the soft-thresholding power
plot(sft$fitIndices[,1], sft$fitIndices[,5],
     xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
     main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")

net = blockwiseModules(datExpr, power = 6,
                       TOMType = "unsigned", minModuleSize = 30,
                       reassignThreshold = 0, mergeCutHeight = 0.25,
                       numericLabels = FALSE, pamRespectsDendro = FALSE,
                       saveTOMs = TRUE,
                       saveTOMFileBase = "femaleMouseTOM",
                       verbose = 3)

# open a graphics window
sizeGrWindow(12, 9)
# Convert labels to colors for plotting
mergedColors = labels2colors(net$colors)
# Plot the dendrogram and the module colors underneath
plotDendroAndColors(net$dendrograms[[1]], mergedColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)

moduleLabels = net$colors
moduleColors = labels2colors(net$colors)
MEs = net$MEs;
geneTree = net$dendrograms[[1]];






#begin fuzzy forest analysis
net = blockwiseModules(datExpr, power = 6,
                       TOMType = "unsigned", minModuleSize = 30,
                       reassignThreshold = 0, mergeCutHeight = 0.25,
                       numericLabels = TRUE, pamRespectsDendro = FALSE,
                       saveTOMs = TRUE,
                       saveTOMFileBase = "femaleMouseTOM",
                       verbose = 3)

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
y <- datTraits$weight_g
X <- datExpr
missing_y <- which(is.na(y))
X <- X[-missing_y, ]
y <- y[-missing_y]
X_impute <- rfImpute(X, y, iter=5, ntree=5000)
n <- dim(X_impute)[1]
samp <- sample(1:n, ceiling(.5*n), replace=TRUE)
imputed_expr <- X_impute[samp, ]
save(imputed_expr, file="imputed_expr.RData")
