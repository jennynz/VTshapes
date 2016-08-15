# Performing Principal Components Analysis on cross-sectional areas

# N.B. Must run readinMRIdata_oneSet.R first!

# Adapted by Jenny Sahng
# 07/08/2016

.libPaths('H:/Documents/Rlibraries')
library('emuR')

# Normalise for maximum area in each shape to eliminate interspeaker effects
# e.g. VT09 shapes much bigger than others.
maxArea=apply(allSpeakers.df[,4:30],1,max)

# Excluding first column X1 because of unreliability of first frame of MRI images (lips are usually poorly defined), and last column X29
pca <- prcomp(~., data=na.omit(allSpeakers.df[,4:30]/maxArea), scale=T)	
pca.summ <- summary(pca)

# Plot of PC1 and PC2, centroids only
plot.new()
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,2]), centroid=T, formant=T, col=T, doellipse=T)
title(main="Centroids of combined vowel properties from 5 vocal tracts", xlab="PC1", ylab="PC2")

# Plot of PC1 and PC2, all vowels
plot.new()
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,2]), centroid=F, formant=T, col=T, doellipse = T)
text(pca$x[,1],pca$x[,2],labels=allSpeakers.df[,2])
title(main="Combined vowel properties from 5 vocal tracts", xlab="PC1", ylab="PC2")

# Plot of standard deviations 
plot(1:length(pca$sdev),pca$sdev,type="p", xlab="Principal component #", ylab="Standard deviation")
title(main="Standard deviations of principal components from combined dataset")

# Plot of proportions of variance
pca.var <- pca.summ$importance[2,]
plot(1:length(pca.var),pca.var, type="p", xlab="Principal component #", ylab="% Variance")
title(main="Proportion of variance explained by principal components")
