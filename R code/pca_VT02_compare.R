# Comparing VT02 against combined dataset ------------------------------

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

# Preamble ---------------------------------------------------------------------

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

# PCA ---------------------------------------------------------------------

all.pca <- prcomp(~., data = allSpeakers.df[,4:30]/maxArea, scale=T)	
all.pca.summ <- summary(all.pca)

noVT2 <- c(1:11,23:132)
combined.pca <- prcomp(~., data = (allSpeakers.df[noVT2,4:30] / maxArea[noVT2]), scale=T)	
combined.pca.summ <- summary(combined.pca)

vt02.pca <- prcomp(~., data = (allSpeakers.df[12:22,4:30] / maxArea[12:22]), scale=T)
vt02.pca.summ <- summary(vt02.pca)

# Plots ---------------------------------------------------------------------

# Plot of PC1 and PC2, centroids of combined dataset with VT02 superimposed
# eplot(combined.pca$x[,1:2], labs=as.character(allSpeakers.df[noVT2,2]), centroid=T, col=F, formant=T, doellipse = F, xlim=c(-5,5), ylim=c(-6,6))
# par(new=TRUE)
# eplot(vt02.pca$x[,1:2], labs=as.character(allSpeakers.df[12:22,2]), centroid=T, col="red", formant=T, doellipse = F, xlim=c(-5,5), ylim=c(-6,6))
# title(main = "Comparison against vowel PCs for NZE and AE", xlab = "PC1", ylab = "PC2")
# legend(x="bottomleft", legend=c("Centroids", "VT02"), col=c("black","red"), lwd=1, pch=c(NA,NA))

# More comparable to vowel quad
# eplot(combined.pca$x[,1:2], labs=as.character(allSpeakers.df[noVT2,2]), centroid=T, col=F, formant=F, doellipse = F, xlim=c(-5,5), ylim=c(-6,6))
# points(vt02.pca$x[,1], vt02.pca$x[,2], pch=as.character(allSpeakers.df[12:22,2]), col="red")

# Bark scaling 
eplot(combined.pca$x[,1:2], labs=as.character(allSpeakers.df[noVT2,2]), axes=F, centroid=T, col=F, doellipse = F, xlim=c(-0.62,-0.46), ylim=c(-0.61,-0.47), scaling = "bark")
par(new=TRUE)
eplot(vt02.pca$x[,1:2], labs=as.character(allSpeakers.df[12:22,2]), axes=F, centroid=T, col="red", xlim=c(-0.62,-0.46), ylim=c(-0.61,-0.47), scaling = "bark")
title(main = "Comparison of vowel PCs in NZE and AE (Bark scaled)", xlab = "PC1", ylab = "PC2")
legend(x=-0.62, y=-0.47, legend=c("NZE Centroids", "VT02"), col=c("black","red"), lwd=1, pch=c(NA,NA))
xticks = seq(-0.62,-0.46,by=0.02)
yticks = seq(-0.60,-0.48,by=0.02)
axis(side = 1, at = xticks, labels = xticks)
axis(side = 2, at = yticks, labels = yticks)

# Try rotating Australian data 90 degrees anticlockwise
# theta <-  -50 * (pi/180)
# rotation.matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), ncol=2, nrow=2, byrow=T)
# vt02rotated <- vt02.pca$x[,1:2] %*% rotation.matrix
# eplot(combined.pca$x[,1:2], labs=as.character(allSpeakers.df[noVT2,2]), axes=F, centroid=T, col=F, doellipse = F, xlim=c(-0.62,-0.46), ylim=c(-0.65,-0.47), scaling = "bark")
# par(new=TRUE)
# eplot(vt02rotated, labs=as.character(allSpeakers.df[12:22,2]), axes=F, centroid=T, col="red", xlim=c(-0.62,-0.46), ylim=c(-0.65,-0.47), scaling = "bark")
# title(main = "Comparison of vowel PCs in NZE and AE (Bark scaled)", xlab = "PC1", ylab = "PC2")
# legend(x=-0.62, y=-0.47, legend=c("NZE Centroids", "VT02"), col=c("black","red"), lwd=1, pch=c(NA,NA))
# xticks = seq(-0.62,-0.46,by=0.02)
# yticks = seq(-0.64,-0.48,by=0.02)
# axis(side = 1, at = xticks, labels = xticks)
# axis(side = 2, at = yticks, labels = yticks)

# F1 vs F2-F1
# f1 <- cbind(combined.pca$x[,1], (combined.pca$x[,2] - combined.pca$x[,1]))
# eplot(f1, labs=as.character(allSpeakers.df[noVT2,2]), centroid=T, col=F, doellipse = F, xlim=c(-0.65,-0.45), ylim=c(-0.65,-0.40), scaling = "bark")
# eplot(f1, labs=as.character(allSpeakers.df[noVT2,2]), centroid=T, col=F, doellipse = F, scaling = "mel")
# par(new=TRUE)
# f1f2 <- cbind(vt02.pca$x[,1], (vt02.pca$x[,2] - vt02.pca$x[,1]))
# eplot(f1f2, labs=as.character(allSpeakers.df[12:22,2]), centroid=T, col="red", xlim=c(-0.65,-0.45), ylim=c(-0.65,-0.40), scaling = "bark")
# eplot(f1f2, labs=as.character(allSpeakers.df[12:22,2]), centroid=T, col="red", scaling = "mel")

# Just try plotting without eplot at all, clusters
# plot(combined.pca$x[,1], combined.pca$x[,2], pch=as.character(allSpeakers.df[noVT2,2]), col="black")
# points(vt02.pca$x[,1], vt02.pca$x[,2], pch=as.character(allSpeakers.df[12:22,2]), col="red")

# Variance ---------------------------------------------------------------------

# Variance accounted for when VT02 is excluded
# all.pca.var <- all.pca.summ$importance[2,]
# combined.pca.var <- combined.pca.summ$importance[2,]
# plot(combined.pca.var, type="p", xlab="Principal component #", ylab="% Variance")
# title(main = "Variance explained by principal components when VT02 is excluded")

# Correlations ---------------------------------------------------------------------

# This doesn't actually work because you can't perform PCA on single-column vectors. 
# Migh be able to use this when we have all the Set 2's.

# Principal component number
p <- 1

# Normalise?
isNorm <- T

# Vowels to analyse: had, heed, HID, hood, who'd
compVowels.i <- c(1, 4, 6, 9, 11)

for (x in compVowels.i) {
  
  # Initialise correlation tables
  corr <- matrix(data = NA, nrow = numVTs-1, ncol = numVTs-1, byrow = TRUE)
  colnames(corr) <- VTlist[-1]
  rownames(corr) <- VTlist[-numVTs]
  
  pval <- matrix(data = NA, nrow = numVTs-1, ncol = numVTs-1, byrow = TRUE)
  colnames(pval) <- VTlist[-1]
  rownames(pval) <- VTlist[-numVTs]
  
  namerow <- matrix(data = NA, nrow = 1, ncol = numVTs-1, byrow = TRUE)
  namerow[1] <- vowelNames[x]
  
  namerowabs <- matrix(data = NA, nrow = 1, ncol = numVTs-1, byrow = TRUE)
  namerowabs[1] <- paste(vowelNames[x], "absolute values", sep=" ")
  
  prow <- matrix(data = NA, nrow = 1, ncol = numVTs-1, byrow = TRUE)
  prow[1] <- paste(vowelNames[x], "P-values", sep=" ")
  
  # Correlations between each of 12 speakers
  for(i in 1:numVTs) {
    j <- i + 1
    while(j <= numVTs) {
      
      # Indices
      m <- ((i-1)*10 + i + (x-1))
      n <- ((j-1)*10 + j + (x-1))
      
      # Individual speaker PCAs with normalised area functions
      if (isNorm == T) {
        pca1 <- prcomp(~., data = allSpeakers.df[m, 4:30] / maxArea[m], scale=T)
        pca2 <- prcomp(~., data = allSpeakers.df[n, 4:30] / maxArea[n], scale=T) 
      } else {
        pca1 <- prcomp(~., data = allSpeakers.df[m, 4:30], scale=T)
        pca2 <- prcomp(~., data = allSpeakers.df[n, 4:30], scale=T) 
      }
      
      # Interspeaker correlations
      cor <- cor.test(pca1$x[,p], pca2$x[,p])
      
      # Write to tables
      corr[i,j-1] <- unname(cor$estimate)
      pvalues[i,j-1] <- unname(cor$p.value)
      
      j <- j + 1
    }
  }  
  
  # Append correlation tables to file
  table <- rbind(namerow, corr, namerowabs, abs(corr), prow, pvalues)
}

# Write table to file
table <- rbind(namerow, corr, namerowabs, abs(corr), prow, pvalues)
if (isNorm == T) {
  write.csv(table, file = paste("pc",p,"_normalised.csv",sep=""))
} else {
  write.csv(table, file = paste("pc",p,"_unnormalised.csv",sep=""))
}
