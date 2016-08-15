# Performing Principal Components Analysis on cross-sectional areas

# N.B. Must run readinMRIdata_5VTs.R first!

# Adapted by Jenny Sahng
# 07/08/2016

.libPaths('H:/Documents/Rlibraries')
library('emuR')

# Normalise for maximum area in each shape to eliminate interspeaker effects
# e.g. VT09 shapes much bigger than others.
# Excluding first column X1 because of unreliability of first frame of MRI images (lips are usually poorly defined), and last column X29
maxArea=apply(allSpeakers.df[,5:30],1,max)

pca <- prcomp(~., data=na.omit(allSpeakers.df[,5:30]/maxArea), scale=T)	
pca.summ <- summary(pca)

# IPA symbols and custom colors for eplot
levels(allSpeakers.df$vow)= c("æ","ɑː","e","iː","ɜː","ɪ","ɔː","ɒ","ʊ","ʌ","u")
colpalette = c("firebrick4","chocolate4","darkgoldenrod","chartreuse4","aquamarine4","darkcyan","deepskyblue4","darkslateblue","darkorchid4","deeppink4","indianred4")

# Plot of PC1 and PC2, centroids only
plot.new()
#eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,3]), centroid=T, formant=F, col=colpalette, doellipse=F) # comparing with Catherine's
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,3]), centroid=T, formant=T, col=colpalette, doellipse=T)
title(main="Centroids of combined vowel properties (5 VTs x 2 sets)", xlab="PC1", ylab="PC2")

# Plot of PC1 and PC2, all vowels
plot.new()
#eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,3]), ylim=c(-5,5), centroid=F, formant=F, col=colpalette, doellipse=F, dopoints=T)
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,3]), centroid=F, formant=T, col=colpalette, doellipse=T, dopoints=T)
title(main="Vowel properties (5 VTs x 2 sets)", xlab="PC1", ylab="PC2")

# Plot of standard deviations 
plot(1:length(pca$sdev),pca$sdev,type="p", xlab="Principal component #", ylab="Standard deviation")
title(main="Standard deviations of principal components from combined dataset (5 VTs x 2 sets)")

# Plot of proportions of variance
pca.var <- pca.summ$importance[2,]
plot(1:length(pca.var),pca.var, type="p", xlab="Principal component #", ylab="% Variance")
title(main="Proportion of variance explained by principal components (5 VTs x 2 sets)")
