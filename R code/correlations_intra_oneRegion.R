# Intra-speaker correlations, one cavity only -----------------------------------------

# Performs Pearson's product-moment correlations between principal component #p 
# of each set within each speakers (the ones which have two datasets available
# and analysed), for just the pharyngeal region

# Written by Jenny Sahng
# 07/09/2016

source('~/Part IV Project/R code/readAreaFunctions_2Set.R')

# Number of principal components to test
np <- 5

# Oral (o) or pharyngeal (p)?
cavity <- "p"
switch(cavity,
  "o" = {
    cavity.index <- c(5:18)
    cavity <- "oral"
  }, "p" = {
    cavity.index <- -c(1:17,32)
    cavity <- "pharyngeal"
  }
)

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
isNorm <- F

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
    set1 <- allSpeakers.df[index1, cavity.index]
    
    set2index <- grep("Set2", allSpeakers.df$set)
    index2 <- intersect(vtindex, set2index)
    set2 <- allSpeakers.df[index2, cavity.index]
    
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
    
  }
  
}

# Write table to file
if (isNorm == T) {
  write.csv(rbind(corr, abs(corr)), file = paste("corr_intra_", cavity, "_norm.csv", sep=""))
} else {
  write.csv(rbind(corr, abs(corr)), file = paste("corr_intra_", cavity, "_unnorm.csv", sep=""))
}
