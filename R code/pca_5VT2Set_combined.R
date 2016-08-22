# Principal Components Analysis on area functions of combined dataset of 12 VTs

# 5 VTs (VT03, VT05, VT08, VT09, VT10), both Set 1 and Set 2.
# Run readAreaFunctions_5VT2Set.R first

# Adapted by Jenny Sahng
# 07/08/2016

.libPaths('H:/Documents/Rlibraries')
library('emuR')

# Normalise for maximum area in each shape to eliminate interspeaker effects
# Excluding first column X1 because of unreliability of first frame of MRI images (lips are usually poorly defined), and last column X29
# e.g. VT09 shapes much bigger than others.
maxArea=apply(allSpeakers.df[,5:30],1,max)

pca <- prcomp(~., data=na.omit(allSpeakers.df[,5:30]/maxArea), scale=T)	
pca.summ <- summary(pca)

# IPA symbols and custom colours for eplot
levels(allSpeakers.df$vow)= c("æ", "????", "e", "i??", "????", "??", "????", "??", "??", "??", "u??")
colpalette = c("firebrick4","chocolate4","darkgoldenrod","chartreuse4","aquamarine4","darkcyan","deepskyblue4","darkslateblue","darkorchid4","deeppink4","indianred4")

# Plot of PC1 and PC2 centroids only
plot.new()
# For comparing with Catherine's plots on powerpoints
#eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,3]), centroid=T, formant=F, col=colpalette, doellipse = F)
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,3]), centroid=T, formant=T, col=colpalette, doellipse = T)
title(main = "Centroids of combined vowel properties (5 VTs x 2 Sets)", xlab = "PC1", ylab = "PC2")

# Plot of PC1 and PC2, all vowels
plot.new()
# For comparing with Catherine's plots on powerpoints
#eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,3]), centroid=F, formant=F, col=colpalette, doellipse = F, dopoints = T, ylim = c(-5,5))
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,3]), centroid=F, formant=T, col=colpalette, doellipse = T, dopoints = T)
title(main = "Vowel properties (5 VTs x 2 Sets)", xlab = "PC1", ylab = "PC2")

# Plot of standard deviations
plot(pca$sdev, type="p", xlab="Principal component #", ylab="Standard deviation accounted")
title(main = "Standard deviations of principal components from combined dataset (5 VTs x 2 Sets)")

# Plot of proportions of variance
pca.var <- pca.summ$importance[2,]
plot(pca.var, type="p", xlab="Principal component #", ylab="% Variance accounted")
title(main = "Proportion of variance explained by principal components (5 VTs x 2 Sets)")
