## Intra-speaker Correlations

# Performs Pearson's product-moment correlations between principal component #p 
# of each set within each speakers (the ones which have two datasets available
# and analysed). When changing the principal component number to compare, make
# sure to change the name of the file that the tables are written to as well.

# Written by Jenny Sahng
# 26/08/2016

source('~/Part IV Project/R code/readAreaFunctions_2Set.R', echo=TRUE)

corr <- matrix(data = NA, nrow = numVTs-1, ncol = numVTs-1, byrow = TRUE)
colnames(corr) <- VTlist[-1]
rownames(corr) <- VTlist[-numVTs]

pvalues <- matrix(data = NA, nrow = numVTs-1, ncol = numVTs-1, byrow = TRUE)
colnames(pvalues) <- VTlist[-1]
rownames(pvalues) <- VTlist[-numVTs]

# Principal component number
p <- 1

# Normalise?
isNorm <- F

# Maximum areas for normalising. X1 is omitted since it is unreliable (MRI
# showed little of mouth opening at front of lips) and X29 omitted since it is
# the glottis which is always zero.
if (isNorm == T) { maxArea <- apply(allSpeakers.df[,4:30],1,max) }

for(i in 1:numVTs) {
    
  # PCAs of each set for one speaker
  vtindex <- grep(VTlist[i], allSpeakers.df$spk)
  
  set1index <- grep("Set1", allSpeakers.df$set)
  index1 <- intersect(vtindex, set1index)
  set1 <- allSpeakers.df[index1,4:30]
  pca1 <- prcomp(~., data = set1/maxArea, scale = T)
  
  set2index <- grep("Set2", allSpeakers.df$set)
  index2 <- intersect(vtindex, set2index)
  set2 <- allSpeakers.df[index2,4:30]
  pca2 <- prcomp(~., data = set2/maxArea, scale = T)
  
  # Interspeaker correlations
  cor <- cor.test(pca1$rotation[,1], pca2$rotation[,1])
  
  # Plots of two area functions
  # plot(c(0,28),c(-0.4,0.4),type="n", xlab="Data point", ylab="Cross-sectional area (mm^2)")
  # lines(pca1$rotation[,1], col="red")
  # lines(pca2$rotation[,1], col="blue")
  # title(main = "PC1 for VT01 and VT02 area function data points")
  # legend(0, -0.3, c("VT01","VT02"), lty=c(1,1), col=c("red","blue"))
  
  cat("\n\t ", VTlist[i],
      "\n\t Correlation: ", cor$estimate,
      "\n\t P-value: ", cor$p.value,
      "\n", sep="")
  
}