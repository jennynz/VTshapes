# Inter-speaker Correlations for AmE and NZE datasets combined

# Performs Pearson's product-moment correlations between principal component p 
# of each speaker with all other speakers.

# Written by Jenny Sahng
# 15/09/2016

rm(list=ls()) # clear workspace
graphics.off() # close all graphics windows

# Parameters ====================================================

path <<- "H:\\Documents\\Part IV Project\\All VT data"

# Final csv file with all information (correlation and variances)
filename <- "NZE_AmE_combined.csv"

# Normalise area functions?
do.norm <- F

# Plot normalised area functions?
do.plots <- F

# Fit a smoothing spline when reading in area functions
do.spline <- F

# The VT and vowel number to skip in correlations if it's missing (SM2 herd)
VT.skip <- "SM2"
Vowel.skip <- "herd"

# Principal components to analyse
p.max <- 3

# Pre-amble ====================================================

source('~/Part IV Project/R code/Story (2005)/readAreaFunctions_Story.R', echo=TRUE)
AmE <- read.Story.data()
source('~/Part IV Project/R code/readAreaFunctions_1Set.R', echo=TRUE)
NZE <- read.NZE.data(path = path, interpN = 44, smooth = do.spline)

# Tests to see if interpolation or smoothing is needed (comment out later)
# NZE1 <- read.NZE.data()
# a<-unname(unlist(NZE$data[22,-(1:2)]))
# b <- smooth.spline(a, tol = 1)
# plot(a, type="l")
# par(new=T)
# plot(b$y, col="red", type="l")
# 
# a<-unname(unlist(NZE$data[22,-(1:2)]))
# b<-unname(unlist(NZE1$data[22,-(1:2)]))
# plot(a, type="l")
# par(new=T)
# plot(b, col="red", type="l")
# 
# a<-unname(unlist(NZE$data[1,-c(1:2,46)]))
# b<-unname(unlist(AmE$data[1,-c(1:2,46)]))
# plot(a, type="l")
# par(new=T)
# plot(b, col="red", type="l")

# Combine datasets
numVTs <- AmE$numVTs + NZE$numVTs
VTlist <- c(NZE$VTlist, AmE$VTlist)

if (do.norm) {
  if (do.norm == "vow") {
    
    # Normalise data specific to each individual vowel
    maxArea.NZE <- apply(NZE$data[,4:45],1,max)
    NZE$data[,3:45] <- NZE$data[,4:45]/maxArea.NZE
    maxArea.AmE <- apply(AmE$data[,3:46],1,max)
    AmE$data[,3:46] <- AmE$data[,3:46]/maxArea.AmE
    
  } else if (do.norm == "spk") {
    
    # Normalise data specific to speaker only
    maxArea <- vector(length = numVTs)
    for (i in 1:numVTs) {
      m <- grep(VTlist[i], combined.df$spk)
      maxArea[i] <- max(combined.df[m,3:46], na.rm = TRUE)
      combined.df[m,3:46] <- combined.df[m,3:46]/maxArea[i]
    }
    
  }
  stopifnot(max(combined.df[,3:46], na.rm=T) == 1)
}

# Switch order of NZE data to match order of vowels in AmE
NZE.switched <- NZE$data
for (i in 1:NZE$numVTs) {
  m <- grep(VTlist[i], NZE$data$spk)
  for (j in 1:length(NZE$vowelNames)) {
    n <- intersect(m, grep(AmE$vowelNames[j], NZE$data$vow))
    k <- m[1] + j - 1
    NZE.switched[k,] <- NZE$data[n,]
  }
}

# Divide NZE by 100 to convert from mm2 to cm2
NZE.switched[,3:46] <- NZE.switched[,3:46]/100

combined.df <- rbind(NZE.switched, AmE$data)

# Plot to visually check that all the area functions are looking in order ==============

if (do.plots) {
  
  # Colour lists
  cols.warm <- c("red", "maroon", "orange", "yellow")
  cols.cold <- c("green1", "green4", "aquamarine3", "aquamarine1", "blue4")
  cols.dark <- c("maroon", "blue4", "blue1", "purple")
  cols.all <- c(cols.warm, cols.cold, cols.dark)
  
  "which.vowels"<-function(vowels="All")
  {
    switch(vowels, 
           "All" = {indices <- 1:11},
           "Front" = {indices <- c(1, 3, 4, 5)},
           "Back" = {indices <- c(2, 7, 8, 9, 10)},
           "Mid" = {indices <- c(5, 6, 9, 11)},
           "High" = {indices <- c(4, 7, 11)},
           "Central" = {indices <- c(1, 3, 5, 6, 10)}
    )
    return(indices)
  }
  
  ## Plot vowels
  selection <- c("All", "Front", "Back", "Mid", "High", "Central")
  
  # One plot per selection
  for (k in selection) {
    
    indices <- which.vowels(vowels = k)
    par(mfrow=c(3,4), lwd=1)
    
    # Each vowel
    for (vow in NZE$vowelNames) {
      m <- grep(vow, combined.df$vow)
      
      if (vow == Vowel.skip) {
        m <- m[m != intersect(m, grep(VT.skip, combined.df$spk))]
      }
      
      plot(1, type="n", axes=T, xlab="normalised distance", ylab="% area", xlim=c(0,1), ylim=c(0,1.05))
      
      # Each speaker
      for (i in 1:length(m)) {
        y <- unname(unlist(combined.df[m[i], 3:46]))
        lines(seq(0,1,1/43), y, col = cols.all[i])
      }
      
      title(vow)
    }
  }
  
}

# Inter-speaker Correlations ====================================================

corr.table <- NA

for (p in 1:p.max) {
  
  # Initialise corrs
  corr <- matrix(data = NA, nrow = numVTs-1, ncol = numVTs-1, byrow = TRUE)
  colnames(corr) <- VTlist[-1]
  rownames(corr) <- VTlist[-numVTs]
  
  pvalues <- matrix(data = NA, nrow = numVTs-1, ncol = numVTs-1, byrow = TRUE)
  colnames(pvalues) <- VTlist[-1]
  rownames(pvalues) <- VTlist[-numVTs]
  
  namerow <- matrix(data = NA, nrow = 1, ncol = numVTs-1, byrow = TRUE)
  namerow[1] <- paste("PC", p, sep="")
  
  namerowabs <- matrix(data = NA, nrow = 1, ncol = numVTs-1, byrow = TRUE)
  namerowabs[1] <- paste("PC", p, " absolute values", sep="")
  
  prow <- matrix(data = NA, nrow = 1, ncol = numVTs-1, byrow = TRUE)
  prow[1] <- paste("PC", p, " p-values", sep="")
  
  for(i in 1:numVTs) {
    
    j <- i + 1
    
    while(j <= numVTs) {
      
      m <- grep(VTlist[i], combined.df$spk)
      n <- grep(VTlist[j], combined.df$spk)
      
      # Skip herd if comparing with SM2 since it doesn't have it.
      if ( (VTlist[i] == VT.skip ) | (VTlist[j] == VT.skip) ) {
        m <- m[m != intersect(m, grep(Vowel.skip, combined.df$vow))]
        n <- n[n != intersect(n, grep(Vowel.skip, combined.df$vow))]
      }
      
      # PCA
      pca1 <- prcomp(~., data = combined.df[m, -c(1:2,46)], scale=T)
      pca2 <- prcomp(~., data = combined.df[n, -c(1:2,46)], scale=T) 
      
      # Interspeaker correlations
      cor <- cor.test(unname(pca1$x[,p]), unname(pca2$x[,p]))
      
      # Write to corrs
      corr[i,j-1] <- unname(cor$estimate)
      pvalues[i,j-1] <- unname(cor$p.value)
      
      j <- j + 1
    }
    
  }
  
  # Append correlation tables
  corr.table <- rbind(corr.table, rbind(namerow, VTlist[-1], corr, namerowabs, VTlist[-1], abs(corr), prow, VTlist[-1], pvalues))
}

write.table(corr.table, file = filename, sep = ",", col.names = F)

# Variances ====================================================

vars.table <- matrix(data = NA, nrow = numVTs, ncol = p.max+2, byrow = TRUE)
colnames(vars.table) <- c(paste("PC", 1:p.max, sep=""), "PC1+PC2", "Total")
rownames(vars.table) <- VTlist

for(i in 1:numVTs) {
  pca <- prcomp(~., data = combined.df[grep(VTlist[i], combined.df$spk), -c(1:2,46)], scale=T)
  vars <- summary(pca)$importance[2,]
  vars.table[i,1:p.max] <- unname(vars[1:p.max])
  vars.table[i,p.max+1] <- vars[1] + vars[2]
  vars.table[i,p.max+2] <- sum(vars[1:p.max])
}

# Write corr to file
write.table(rbind(colnames(vars.table), vars.table), file = filename, append = TRUE, sep = ",", col.names = F)

# Plot calculated variances and variances from Story
plot(100 * vars.table[,p.max+2], col="red", ylim=c(85,100))
par(new=T)
points(c(92.8,90.5,94.7,93,92.6,97.3))

# Plot PC1-PC2 vowel centroids
