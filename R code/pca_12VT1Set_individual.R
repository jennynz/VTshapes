# Principal Components Analysis on individual speaker area functions

# All 12 VTs, 1 Set each (Set 1), PCA performed on each speaker individually.
# Run readAreaFunctions_12VT1Set.R first

# Adapted by Jenny Sahng
# 07/08/2016

.libPaths('H:/Documents/Rlibraries')
library('emuR')

# Normalise for maximum area in each shape to eliminate interspeaker effects
# Excluding first column X1 because of unreliability of first frame of MRI images (lips are usually poorly defined), and last column X29
# e.g. VT09 shapes much bigger than others.
maxArea <- apply(allSpeakers.df[,4:30],1,max)

# PCA of each individual speaker
pca <- NULL
for(i in numVTs){
  spk <- VTlist[i]
  spkData <- allSpeakers.df[grep(spk, allSpeakers.df$spk),4:30]
  spkPCA <- prcomp(~., data=na.omit(allSpeakers.df[,4:30]/maxArea), scale=T)	
  pca[i] <- spkPCA
}

# Correlations between each speaker


# IPA symbols and custom colours for eplot
levels(allSpeakers.df$vow) <- c("æ", "????", "e", "i??", "????", "??", "????", "??", "??", "??", "u??")
colpalette <- c("firebrick4","chocolate4","darkgoldenrod","chartreuse4","aquamarine4","darkcyan","deepskyblue4","darkslateblue","darkorchid4","deeppink4","indianred4")

# Plot of PC1 and PC2, centroids only
plot.new()
#eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,2]), centroid=T, formant=F, col=colpalette, doellipse = F)
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,2]), centroid=T, formant=T, col=colpalette, doellipse = T)
title(main="Centroids of combined vowel properties (12 VTs x 1 Set)", xlab="PC1", ylab="PC2")

# Plot of PC1 and PC2, all vowels
plot.new()
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,2]), centroid=F, formant=T, col=colpalette, doellipse = T, dopoints = T)
title(main="Combined vowel properties (12 VTs x 1 Set)", xlab="PC1", ylab="PC2")

# Plot of standard deviations 
plot(1:length(pca$sdev),pca$sdev,type="p", xlab="Principal component #", ylab="Standard deviation")
title(main="Standard deviations of principal components from combined dataset")

# Plot of proportions of variance
pca.var <- pca.summ$importance[2,]
plot(1:length(pca.var),pca.var, type="p", xlab="Principal component #", ylab="% Variance")
title(main="Proportion of variance explained by principal components")
