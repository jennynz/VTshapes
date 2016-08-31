# Comparing VT02 against combined dataset

# The interspeaker correlation between VT02 and the other VTs was strangely low.
# Given that the participant was Australian within an NZE dataset, this script 
# plots VT02's vowel in the PC space on top of the centroids of the combined 
# dataset excluding VT02. Given that NZE hid and hood vowels are more 
# centralizing and lower than AE, while front high vowels are higher, we should 
# expect a similar result here with VT02, although only having a sample size of 
# 1 for Australian English, this is hard to determine. A more robust analysis 
# would be including VT02 in an interspeaker correlation analysis with other 
# Australian English speakers with the hypothesis that there would be no 
# significant differences in its correlation values than with all the other AE
# speakers. This would require MRI data of Australian speakers captured with the
# same methods.

# Written by Jenny Sahng
# 31/08/2016

source('~/Part IV Project/R code/readAreaFunctions_1Set.R', echo=TRUE)

.libPaths('H:/Documents/Rlibraries')
library('emuR')

# Normalise for maximum area in each shape to eliminate interspeaker effects 
# Excluding first column X1 because of unreliability of first frame of MRI
# images (lips are usually poorly defined), and last column X29 e.g. VT09 shapes
# much bigger than others.
maxArea <- apply(allSpeakers.df[,4:30],1,max)

pca <- prcomp(~., data=na.omit(allSpeakers.df[,4:30]/maxArea), scale=T)	
pca.summ <- summary(pca)

# IPA symbols and custom colours for eplot. Paste in vowels from file 'IPAvowels'
levels(allSpeakers.df$vow) <- c()
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
