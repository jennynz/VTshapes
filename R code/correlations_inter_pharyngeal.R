# Inter-speaker correlations, Pharyngeal only -----------------------------------------

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


# Perform interspeaker correlations on only the pharyngeal region.

# Written by Jenny Sahng
# 7/09/2016

source('~/Part IV Project/R code/readAreaFunctions_1Set.R', echo=TRUE)

# Principal components to analyse
p.max <- 3

# Normalise?
isNorm <- T

# Maximum areas for normalising. X1 is omitted since it is unreliable (MRI
# showed little of mouth opening at front of lips) and X29 omitted since it is
# the glottis which is always zero.
if (isNorm == T) { maxArea <- apply(allSpeakers.df[,4:30],1,max) }

table <- NA

for (p in 1:p.max) {
  
  # Initialise tables
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
      
      m <- ((i-1)*10 + i):(i*10 + i)
      n <- ((j-1)*10 + j):(j*10 + j)
      
      # Individual speaker PCAs with normalised area functions
      if (isNorm == T) {
        pca1 <- prcomp(~., data = allSpeakers.df[m, -c(1:16,31)]/maxArea[m], scale=T)
        pca2 <- prcomp(~., data = allSpeakers.df[n, -c(1:16,31)]/maxArea[n], scale=T) 
      } else {
        pca1 <- prcomp(~., data = allSpeakers.df[m, -c(1:16,31)], scale=T)
        pca2 <- prcomp(~., data = allSpeakers.df[n, -c(1:16,31)], scale=T) 
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
  table <- rbind(table, rbind(namerow, corr, namerowabs, abs(corr), prow, pvalues))
}

if (isNorm == T) {
  write.csv(table, file = "corr_inter_pharyngeal_normalised.csv")
} else {
  write.csv(table, file = "corr_inter_pharyngeal_unnormalised.csv")
}
