## Inter-speaker Correlations

# Performs Pearson's product-moment correlations between principal component p 
# of each speaker with all other speakers.

# Written by Jenny Sahng
# 16/08/2016

source('~/Part IV Project/R code/readAreaFunctions_1Set.R', echo=TRUE)

corr <- matrix(data = NA, nrow = numVTs-1, ncol = numVTs-1, byrow = TRUE)
colnames(corr) <- VTlist[-1]
rownames(corr) <- VTlist[-numVTs]

pvalues <- matrix(data = NA, nrow = numVTs-1, ncol = numVTs-1, byrow = TRUE)
colnames(pvalues) <- VTlist[-1]
rownames(pvalues) <- VTlist[-numVTs]

# Principal component number
p <- 2

# Normalise?
isNorm <- T

# Maximum areas for normalising. X1 is omitted since it is unreliable (MRI
# showed little of mouth opening at front of lips) and X29 omitted since it is
# the glottis which is always zero.
if (isNorm == T) { maxArea <- apply(allSpeakers.df[,4:30],1,max) }

for(i in 1:numVTs) {
  j <- i + 1
  while(j <= numVTs) {
    
    # Individual speaker PCAs with normalised area functions
    if (isNorm == T) {
      pca1 <- prcomp(~., data = allSpeakers.df[((i-1)*10 + i):(i*10 + i),4:30]/maxArea[((i-1)*10 + i):(i*10 + i)], scale=T)
      pca2 <- prcomp(~., data = allSpeakers.df[((j-1)*10 + j):(j*10 + j),4:30]/maxArea[((j-1)*10 + j):(j*10 + j)], scale=T) 
    } else {
      pca1 <- prcomp(~., data = allSpeakers.df[((i-1)*10 + i):(i*10 + i),4:30], scale=T)
      pca2 <- prcomp(~., data = allSpeakers.df[((j-1)*10 + j):(j*10 + j),4:30], scale=T) 
    }
    
    # Normality tests
    # shapiro.test(pca1$rotation[,p])
    # shapiro.test(pca2$rotation[,p])
    # qqnorm(pca1$rotation[,p])
    # qqnorm(pca2$rotation[,p])
    
    # Heteroscedasticity tests
    #lmtest::bptest(pca1$rotation[,p])
    #car::ncvTest(pca1$rotation[,p])
    
    # Interspeaker correlations
    cor <- cor.test(pca1$rotation[,p], pca2$rotation[,p])
    
    # Write to tables
    corr[i,j-1] <- unname(cor$estimate)
    pvalues[i,j-1] <- unname(cor$p.value)
    
    # Print to console
    # cat("\n\t VT", i, " and VT", j,
    #   "\n\t Correlation: ", cor$estimate,
    #   "\n\t P-value: ", cor$p.value,
    #   "\n", sep="")
    
    # Plots two example area functions
    # plot(c(0,28),c(-0.4,0.4),type="n", xlab="Data point", ylab="Cross-sectional area (mm^2)")
    # lines(pca1$rotation[,1], col="red")
    # lines(pca2$rotation[,1], col="blue")
    # title(main = "PC1 for VT01 and VT02 area function data points")
    # legend(0, -0.3, c("VT01","VT02"), lty=c(1,1), col=c("red","blue"))
    
    j <- j + 1
  }
}

# Write table to file
table <- rbind(corr, abs(corr), pvalues)
if (isNorm == T) {
  write.csv(table, file = paste("pc",p,"_normalised.csv",sep=""))
} else {
  write.csv(table, file = paste("pc",p,"_unnormalised.csv",sep=""))
}
