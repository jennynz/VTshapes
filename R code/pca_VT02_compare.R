# Comparing VT02 against combined dataset

# The interspeaker correlation between VT02 and the other VTs was strangely low.
# Given that the participant was Australian within an NZE dataset, this script 
# plots VT02's vowel in the PC space on top of the centroids of the combined 
# dataset excluding VT02. Given that NZE hid and hood vowels are more 
# centralizing and lower than AE, while front high vowels are higher (Watson,
# Harrington & Evans, 1998), we should expect a similar result here with VT02,
# although only having a sample size of 1 for Australian English, this is hard
# to determine. A more robust analysis would be including VT02 in an
# interspeaker correlation analysis with other Australian English speakers with
# the hypothesis that there would be no significant differences in its
# correlation values than with all the other AE speakers. This would require MRI
# data of Australian speakers captured with the same methods.

# Written by Jenny Sahng
# 31/08/2016

source('~/Part IV Project/R code/readAreaFunctions_1Set.R', echo=TRUE)

# IPA symbols and custom colours for eplot. Paste in vowels from file 'IPAvowels'
levels(allSpeakers.df$vow) <- c()

.libPaths('H:/Documents/Rlibraries')
library('emuR')

# Normalise for maximum area in each shape to eliminate interspeaker effects 
# Excluding first column X1 because of unreliability of first frame of MRI
# images (lips are usually poorly defined), and last column X29 e.g. VT09 shapes
# much bigger than others.
maxArea <- apply(allSpeakers.df[,4:30],1,max)

# PCA
all.pca <- prcomp(~., data = allSpeakers.df[,4:30]/maxArea, scale=T)	
all.pca.summ <- summary(all.pca)

noVT2 <- c(1:11,23:132)
combined.pca <- prcomp(~., data = (allSpeakers.df[noVT2,4:30] / maxArea[noVT2]), scale=T)	
combined.pca.summ <- summary(combined.pca)

vt02.pca <- prcomp(~., data = (allSpeakers.df[12:22,4:30] / maxArea[12:22]), scale=T)
vt02.pca.summ <- summary(vt02.pca)

# Plot of PC1 and PC2, centroids of combined dataset with VT02 superimposed
eplot(combined.pca$x[,1:2], labs=as.character(allSpeakers.df[noVT2,2]), centroid=T, col=F, formant=T, doellipse = F, xlim=c(-5,5), ylim=c(-6,6))
par(new=TRUE)
eplot(vt02.pca$x[,1:2], labs=as.character(allSpeakers.df[12:22,2]), centroid=T, col="red", formant=T, doellipse = F, xlim=c(-5,5), ylim=c(-6,6))
title(main = "Comparison against vowel PCs for NZE and AE", xlab = "PC1", ylab = "PC2")
legend(x="bottomleft", legend=c("Centroids", "VT02"), col=c("black","red"), lwd=1, pch=c(NA,NA))

# More comparable to vowel quad
eplot(combined.pca$x[,1:2], labs=as.character(allSpeakers.df[noVT2,2]), centroid=T, col=F, formant=F, doellipse = F, xlim=c(-5,5), ylim=c(-6,6))
points(vt02.pca$x[,1], vt02.pca$x[,2], pch=as.character(allSpeakers.df[12:22,2]), col="red")

# Bark scaling
eplot(combined.pca$x[,1:2], labs=as.character(allSpeakers.df[noVT2,2]), centroid=T, col=F, doellipse = F, xlim=c(-0.65,-0.45), ylim=c(-0.65,-0.45), scaling = "bark")
par(new=TRUE)
eplot(vt02.pca$x[,1:2], labs=as.character(allSpeakers.df[12:22,2]), centroid=T, col="red", xlim=c(-0.65,-0.45), ylim=c(-0.65,-0.45), scaling = "bark")

# F1 vs F2-F1
f1 <- cbind(combined.pca$x[,1], (combined.pca$x[,2] - combined.pca$x[,1]))
eplot(f1, labs=as.character(allSpeakers.df[noVT2,2]), centroid=T, col=F, doellipse = F, xlim=c(-0.65,-0.45), ylim=c(-0.65,-0.40), scaling = "bark")
eplot(f1, labs=as.character(allSpeakers.df[noVT2,2]), centroid=T, col=F, doellipse = F, scaling = "mel")
par(new=TRUE)
f1f2 <- cbind(vt02.pca$x[,1], (vt02.pca$x[,2] - vt02.pca$x[,1]))
eplot(f1f2, labs=as.character(allSpeakers.df[12:22,2]), centroid=T, col="red", xlim=c(-0.65,-0.45), ylim=c(-0.65,-0.40), scaling = "bark")
eplot(f1f2, labs=as.character(allSpeakers.df[12:22,2]), centroid=T, col="red", scaling = "mel")

# Just try plotting without eplot at all, clusters
plot(combined.pca$x[,1], combined.pca$x[,2], pch=as.character(allSpeakers.df[noVT2,2]), col="black")
points(vt02.pca$x[,1], vt02.pca$x[,2], pch=as.character(allSpeakers.df[12:22,2]), col="red")

# Variance accounted for when VT02 is excluded
all.pca.var <- all.pca.summ$importance[2,]
combined.pca.var <- combined.pca.summ$importance[2,]
plot(combined.pca.var, type="p", xlab="Principal component #", ylab="% Variance")
title(main = "Variance explained by principal components when VT02 is excluded")