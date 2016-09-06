## Intra-speaker Correlations

# Performs Pearson's product-moment correlations between principal component #p 
# of each set within each speakers (the ones which have two datasets available
# and analysed).

# Written by Jenny Sahng
# 26/08/2016

source('~/Part IV Project/R code/readAreaFunctions_2Set.R', echo=TRUE)

# Number of principal components to test
np <- 5

# Table with VTs along columns, and two rows showing correlation estimate
# between two sets and p-value respectively, for each principal component.
corr <- matrix(data = NA, nrow = 2*np, ncol = numVTs, byrow = TRUE)
colnames(corr) <- VTlist
rowlist <- 0
for (i in 1:np) {
  rowlist[i*2-1] <- paste("PC", i, " Corr", sep="")
  rowlist[i*2] <- paste("PC", i, " P-value", sep="")
}
rownames(corr) <- rowlist

# Normalise?
isNorm <- T

# Maximum areas for normalising. X1 is omitted since it is unreliable (MRI
# showed little of mouth opening at front of lips) and X29 omitted since it is
# the glottis which is always zero.
if (isNorm == T) { maxArea <- apply(allSpeakers.df[,4:30],1,max) }

for (p in 1:np) {
  for(i in 1:numVTs) {
      
    # Get area functions for both sets of within same speaker
    vtindex <- grep(VTlist[i], allSpeakers.df$spk)
    
    set1index <- grep("Set1", allSpeakers.df$set)
    index1 <- intersect(vtindex, set1index)
    set1 <- allSpeakers.df[index1,4:30]
    
    set2index <- grep("Set2", allSpeakers.df$set)
    index2 <- intersect(vtindex, set2index)
    set2 <- allSpeakers.df[index2,4:30]
    
    # Normalise
    if (isNorm == T) {
      set1 <- set1/maxArea[index1]
      set2 <- set2/maxArea[index2]
    }
    
    # PCAs of each set for one speaker
    pca1 <- prcomp(~., data = set1, scale = T)
    pca2 <- prcomp(~., data = set2, scale = T)
    
    # intraspeaker correlations
    cor <- cor.test(pca1$x[,p], pca2$x[,p])
    
    # Write to tables
    corr[p*2-1,i] <- unname(cor$estimate)
    corr[p*2,i] <- unname(cor$p.value)
    
    # Plots of two area functions
    # plot(c(0,28),c(-0.4,0.4),type="n", xlab="Data point", ylab="Cross-sectional area (mm^2)")
    # lines(pca1$rotation[,1], col="red")
    # lines(pca2$rotation[,1], col="blue")
    # title(main = "PC1 for VT01 and VT02 area function data points")
    # legend(0, -0.3, c("VT01","VT02"), lty=c(1,1), col=c("red","blue"))
    
    # Print to console
    # cat("\n\t ", VTlist[i],
    #     "\n\t Correlation: ", cor$estimate,
    #     "\n\t P-value: ", cor$p.value,
    #     "\n", sep="")
    
  }
  
}

# Write table to file
if (isNorm == T) {
  write.csv(rbind(corr, abs(corr)), file = "correlations_intra_norm.csv")
} else {
  write.csv(rbind(corr, abs(corr)), file = "correlations_intra.csv")
}
