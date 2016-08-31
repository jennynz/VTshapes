# Principal Components Analysis on area functions of combined dataset & Plots

# All 12 VTs, Set 1 only for each.

# Adapted by Jenny Sahng
# 07/08/2016

source('~/Part IV Project/R code/readAreaFunctions_1Set.R', echo=TRUE)

.libPaths('H:/Documents/Rlibraries')
library('emuR')

# Normalise for maximum area in each shape to eliminate interspeaker effects
# Excluding first column X1 because of unreliability of first frame of MRI images (lips are usually poorly defined), and last column X29
# e.g. VT09 shapes much bigger than others.
maxArea <- apply(allSpeakers.df[,4:30],1,max)

pca <- prcomp(~., data=na.omit(allSpeakers.df[,4:30]/maxArea), scale=T)	
pca.summ <- summary(pca)

# IPA symbols and custom colours for eplot
levels(allSpeakers.df$vow) <- c("æ", "????", "e", "i??", "????", "??", "????", "??", "??", "??", "u??")
colpalette <- c("firebrick4","chocolate4","darkgoldenrod","chartreuse4","aquamarine4","darkcyan","deepskyblue4","darkslateblue","darkorchid4","deeppink4","indianred4")

# Plot of PC1 and PC2, all vowels
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,2]), centroid=F, formant=T, col=colpalette, doellipse = T, dopoints = T)
title(main = "Vowel properties (12 VTs x 1 Sets)", xlab = "PC1", ylab = "PC2")
# Compare with old 12VT1Set plot where hood was huddled in corner.
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,2]), centroid=F, formant=T, col=colpalette, doellipse = F, dopoints = T, xlim = c(-6,6), ylim = c(-6, 6))
title(main = "Vowel properties (12 VTs x 1 Sets)", xlab = "PC1", ylab = "PC2")

# Plot of PC1 and PC2, centroids only
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,2]), centroid=T, formant=T, col=colpalette, doellipse = T)
title(main = "Centroids of combined vowel properties (12 VTs x 1 Set)", xlab = "PC1", ylab = "PC2")

# Plot of standard deviations 
plot(pca$sdev, type="p", xlab="Principal component #", ylab="Standard deviation")
title(main = "Standard deviations of principal components from combined dataset (12 VTs x 1 Set)")

# Plot of proportions of variance
pca.var <- pca.summ$importance[2,]
plot(pca.var, type="p", xlab="Principal component #", ylab="% Variance")
title(main = "Proportion of variance explained by principal components (12 VTs x 1 Set)")

# VT02 head: Comparing linearly interpolated area function with raw distance_area txt from MATLAB
plot (c(0,29),c(0,300),type="n", xlab="Data point", ylab="Cross-sectional area (mm^2)")
lines(unlist(allSpeakers.df[14,-(1:2)]), col="red")
vt02e <- read.table(paste(path,VTlist[2],"Set1","distance_area","head.txt",sep="\\"))
lines(vt02e[,2], col="blue")
title(main = "VT02 'head' vowel area functions")
legend(0, 40, c("Interpolated on R","Raw area function"), lty=c(1,1), col=c("red","blue"))

# VT09 had: Comparing linearly interpolated area function with raw distance_area txt from MATLAB
plot (c(0,29),c(0,700),type="n", xlab="Data point", ylab="Cross-sectional area (mm^2)")
lines(unlist(allSpeakers.df[89,-(1:2)]), col="red")
vt02e <- read.table(paste(path,VTlist[9],"Set1","distance_area","had.txt",sep="\\"))
lines(vt02e[,2], col="blue")
title(main = "VT09 'had' vowel area functions")
legend(0, 100, c("Interpolated on R","Raw area function"), lty=c(1,1), col=c("red","blue"))
