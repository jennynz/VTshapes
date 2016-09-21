# Inter-speaker Correlations between resonances

# Performs Pearson's product-moment correlations between resonances
# of each speaker with all other speakers.

# Written by Jenny Sahng
# 19/09/2016

# Parameters ===================================================================

# Final csv file with all information (correlation and variances)
filename <- "resonances_correlations.csv"

# Number of resonances or principal components to analyse
p.max <- numRes

# Do PCA on the resonances before correlation analysis?
do.pca <- F

# Inter-speaker Correlations ====================================================

res.df = data.frame(spk = factor(rep(VTlist, each=length(vowelNames))), vow = factor(rep(vowelNames, numVTs)), resfreq)

corr.table <- NA

for (p in 1:p.max) {
  
  # Initialise corrs
  corr <- matrix(data = NA, nrow = numVTs-1, ncol = numVTs-1, byrow = TRUE)
  colnames(corr) <- VTlist[-1]
  rownames(corr) <- VTlist[-numVTs]
  
  pvalues <- matrix(data = NA, nrow = numVTs-1, ncol = numVTs-1, byrow = TRUE)
  colnames(pvalues) <- VTlist[-1]
  rownames(pvalues) <- VTlist[-numVTs]
  
  if (do.pca) {
    t <- "PC"
  } else {
    t <- "R"
  }
  
  namerow <- matrix(data = NA, nrow = 1, ncol = numVTs-1, byrow = TRUE)
  namerow[1] <- paste(t, p, sep="")
  
  namerowabs <- matrix(data = NA, nrow = 1, ncol = numVTs-1, byrow = TRUE)
  namerowabs[1] <- paste(t, p, " absolute values", sep="")
  
  prow <- matrix(data = NA, nrow = 1, ncol = numVTs-1, byrow = TRUE)
  prow[1] <- paste(t, p, " p-values", sep="")
  
  for(i in 1:numVTs) {
    
    j <- i + 1
    
    while(j <= numVTs) {
      
      m <- grep(VTlist[i], res.df$spk)
      n <- grep(VTlist[j], res.df$spk)
      
      # Skip herd if comparing with SM2 since it doesn't have it.
      if ( (VTlist[i] == VT.skip ) | (VTlist[j] == VT.skip) ) {
        m <- m[m != intersect(m, grep(paste("\\b",Vowel.skip,"\\b",sep=""), res.df$vow))]
        n <- n[n != intersect(n, grep(paste("\\b",Vowel.skip,"\\b",sep=""), res.df$vow))]
      }
      
      # PCA
      if (do.pca) {
        pca1 <- prcomp(~., data = res.df[m, -c(1:2)], scale=do.scale)
        pca2 <- prcomp(~., data = res.df[n, -c(1:2)], scale=do.scale) 
        a <- unname(pca1$x[,p])
        b <- unname(pca2$x[,p])
      } else {
        a <- res.df[m,2+p]
        b <- res.df[n,2+p]
      }
      
      # Interspeaker correlations
      cor <- cor.test(a, b)
      
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

if (do.pca) {
  
  vars.table <- matrix(data = NA, nrow = numVTs, ncol = p.max+2, byrow = TRUE)
  colnames(vars.table) <- c(paste("PC", 1:p.max, sep=""), "PC1+PC2", "Total")
  rownames(vars.table) <- VTlist
  
  for(i in 1:numVTs) {
    pca <- prcomp(~., data = res.df[grep(VTlist[i], res.df$spk), -c(1:2,46)], scale=do.scale)
    vars <- summary(pca)$importance[2,]
    vars.table[i,1:p.max] <- unname(vars[1:p.max])
    vars.table[i,p.max+1] <- vars[1] + vars[2]
    vars.table[i,p.max+2] <- sum(vars[1:p.max])
  }
  
  # Write corr to file
  write.table(rbind(colnames(vars.table), vars.table), file = filename, append = TRUE, sep = ",", col.names = F)
  
}