# Principal Components Analysis on area functions of combined dataset & Plots

# All VTs for each accent group, Set 1 only for each.

# Adapted by Jenny Sahng
# 07/08/2016


rm(list=ls()) # clear workspace
graphics.off() # close all graphics windows

# Normalise area functions? (F, "spk" or "vow")
do.norm <- "vow"

# NZE or AmE?
set <- "NZE"

path <<- "H:\\Documents\\Part IV Project\\All VT data"

if (set == "NZE") {
  source('~/Part IV Project/R code/readAreaFunctions_1Set.R')
  spkdata <- read.NZE.data()
  r <- 3:30
} else if (set == "AmE") {
  source('~/Part IV Project/R code/Story (2005)/readAreaFunctions_Story.R')
  spkdata <- read.Story.data()
  r <- 3:46
  setwd("~/Part IV Project/R code")
}

allSpeakers.df <- spkdata$data
VTlist <- spkdata$VTlist
numVTs <- spkdata$numVTs
vowelNames <- spkdata$vowelNames
allSpeakers.df$vow <- factor(allSpeakers.df$vow, levels = vowelNames)
levels(allSpeakers.df$vow) <- c() # Copy and paste IPA characters from IPAvowels.txt for NZE and IPAvowels_story.txt for AmE

colpalette <- c("firebrick4","chocolate4","darkgoldenrod","chartreuse4","aquamarine4","darkcyan","deepskyblue4","darkslateblue","darkorchid4","deeppink4","indianred4")

.libPaths('H:/Documents/Rlibraries')
library('emuR')

if (do.norm == "spk") {
  # Normalise data specific to speaker only
  maxArea <- vector(length = numVTs)
  for (i in 1:numVTs) {
    m <- grep(VTlist[i], allSpeakers.df$spk)
    maxArea[i] <- max(allSpeakers.df[m,r], na.rm = TRUE)
    allSpeakers.df[m,r] <- allSpeakers.df[m,r]/maxArea[i]
  }
} else if (do.norm == "vow") {
  # Normalise data specific to each individual vowel
  maxArea <- apply(allSpeakers.df[,r],1,max)
  allSpeakers.df[,r] <- allSpeakers.df[,r]/maxArea
}

# get rid of NA line in AmE
if (set == "AmE") {
  allSpeakers.df <- allSpeakers.df[-48,]
}

# PCA
pca <- prcomp(~., data = allSpeakers.df[,r], scale=T)	

pca.summ <- summary(pca)

# Most vowel-quad-like
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,2]), centroid=T, formant=F, col=colpalette,
      doellipse = F, xlim = c(6,-6), ylim = c(4, -4))
title(main = "Vowel centroids on unrotated PC1-PC2 planes", xlab = "PC1", ylab = "PC2")

# For poster
colpalette <- c("chartreuse4","slategray4","slategray4","chartreuse4","slategray4","slategray4",
                "slategray4","chartreuse4","slategray4","slategray4","chartreuse4")
par(cex.axis=1, cex.lab=1, cex.main=1, cex=2.5, col.axis="#1d3c6d", col.lab="#1d3c6d", col.main="#1d3c6d")
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,2]), centroid=T, formant=F, col=colpalette,
      doellipse = F, xlim = c(6,-5), ylim = c(2.5, -3))
box(col = "#1d3c6d")
title(main = "Average PC values for 18 speakers", xlab = "PC1", ylab = "PC2")

# Plot of PC1 and PC2, all vowels
colpalette <- c("firebrick4","chocolate4","darkgoldenrod","chartreuse4","aquamarine4",
                "darkcyan","deepskyblue4","darkslateblue","darkorchid4","deeppink4","indianred4")
par(cex=1)
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,2]), centroid=F, formant=T, col=colpalette,
      doellipse = F, dopoints = T)
title(main = "Vowels on PC1-PC2 planes (12 VTs x 1 Sets)", xlab = "PC1", ylab = "PC2")

# Compare with old 12VT1Set plot where hood was huddled in corner.
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,2]), centroid=F, formant=T, col=colpalette,
      doellipse = F, dopoints = T, xlim = c(-6,6), ylim = c(-6, 6))
title(main = "Vowels on PC1-PC2 planes (12 VTs x 1 Sets)", xlab = "PC1", ylab = "PC2")

# Plot of PC1 and PC2, centroids only
eplot(pca$x[,1:2], labs=as.character(allSpeakers.df[,2]), centroid=T, formant=T, col=colpalette,
      doellipse = F, xlim = c(8,-8), ylim = c(8,-8))
title(main = "Vowel centroids on PC1-PC2 planes (12 VTs x 1 Set)", xlab = "PC1", ylab = "PC2")

# Gender
f <- grep('^(SF|VT02|VT06|VT07|VT08|VT11|VT12)', allSpeakers.df$spk)
m <- grep('^(SM|VT01|VT03|VT04|VT05|VT9|VT10)', allSpeakers.df$spk)

eplot(pca$x[f,1:2], labs=as.character(allSpeakers.df[f,2]), centroid=T, formant=F, col="deeppink",
      font=2, doellipse = F, xlim = c(8,-9), ylim = c(4, -5))
par(new=T)
eplot(pca$x[m,1:2], labs=as.character(allSpeakers.df[m,2]), centroid=T, formant=F, col="dodgerblue",
      font=2, doellipse = F, xlim = c(8,-9), ylim = c(4, -5))
title(main = "Vowel centroids on PC1-PC2 planes for GenAm female and male speakers", xlab = "PC1", ylab = "PC2")
legend("bottomright", bty="n", c("Female","Male"), lty=c(1,1), lwd=c(2,2), col=c("deeppink", "dodgerblue"))

# Plot of standard deviations 
plot(pca$sdev, type="p", xlab="Principal component #", ylab="Standard deviation")
title(main = "Standard deviations of principal components from combined dataset (12 VTs x 1 Set)")

# Plot of proportions of variance
pca.var <- pca.summ$importance[2,]
plot(pca.var, type="p", xlab="Principal component #", ylab="% Variance")
title(main = "Proportion of variance explained by PCs")

# Heed (VT04 vs VT09): Comparing linearly interpolated area function with raw distance_area txt from MATLAB

## Before normalisation, just comparing the  raw area functions (what I had in the draft report)
plot (c(0,29), c(0,700), type="n", xlab="Data point", ylab=expression(Cross-sectional ~ area ~ (mm^{2})), xlim=c(1,29))

# VT04
lines(unlist(spkdata$data[37,-(1:2)]), col="red", lwd = 2)
vt02e <- read.table(paste(path,VTlist[4],"Set1","distance_area","heed.txt",sep="\\"))
lines(vt02e[,2], col="firebrick3", lwd = 2)

# VT09
lines(unlist(spkdata$data[92,-(1:2)]), col="blue", lwd = 2)
vt09e <- read.table(paste(path,VTlist[9],"Set1","distance_area","heed.txt",sep="\\"))
lines(vt09e[,2], col="deepskyblue3", lwd = 2)

# Plot details
title(main = "Raw and linearly interpolated area functions for 'had' vowel")
legend("topleft", bty="n", c("VT04 interpolated","VT04 raw data", "VT09 interpolated", "VT09 raw data"), lty=c(1,1), col=c("red","firebrick3","blue","deepskyblue3"))

## When normalised for vowels
if (do.norm == "vow") { ylims <- c(0,1) } else { ylims <- c(0,700) }
plot (c(0,29),ylims,type="n", xlab="Data point", ylab=expression(Cross-sectional ~ area ~ (mm^{2})), xlim=c(1,29))

# VT04
lines(unlist(allSpeakers.df[37,-(1:2)]), col="red", lwd = 2)
vt02e <- read.table(paste(path,VTlist[4],"Set1","distance_area","heed.txt",sep="\\"))
if (do.norm == "vow") { vt02e[,2] <- vt02e[,2]/max(vt02e[,2]) }
lines(vt02e[,2], col="firebrick3", lwd = 2)

# VT09
lines(unlist(allSpeakers.df[92,-(1:2)]), col="blue", lwd = 2)
vt09e <- read.table(paste(path,VTlist[9],"Set1","distance_area","heed.txt",sep="\\"))
if (do.norm == "vow") { vt09e[,2] <- vt09e[,2]/max(vt09e[,2]) }
lines(vt09e[,2], col="deepskyblue3", lwd = 2)

# Plot details
title(main = "Raw and linearly interpolated area functions for 'had' vowel")
legend("topleft", bty="n", c("VT04 interpolated","VT04 raw data", "VT09 interpolated", "VT09 raw data"), lty=c(1,1), col=c("red","firebrick3","blue","deepskyblue3"))
