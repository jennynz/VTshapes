## Inter-speaker Correlations

# Performs Pearson's product-moment correlations between principal component p 
# of each speaker with all other speakers.

# Written by Jenny Sahng
# 16/08/2016

rm(list=ls()) # clear workspace
graphics.off() # close all graphics windows
source('~/Part IV Project/R code/readAreaFunctions_1Set.R', echo=TRUE)
NZE <- read.NZE.data()
numVTs <- NZE$numVTs
VTlist <- NZE$VTlist

corr <- matrix(data = NA, nrow = numVTs-1, ncol = numVTs-1, byrow = TRUE)
colnames(corr) <- VTlist[-1]
rownames(corr) <- VTlist[-numVTs]

pvalues <- matrix(data = NA, nrow = numVTs-1, ncol = numVTs-1, byrow = TRUE)
colnames(pvalues) <- VTlist[-1]
rownames(pvalues) <- VTlist[-numVTs]

# Principal component number
p <- 1

# Normalise?
isNorm <- T

# Maximum areas for normalising. X1 is omitted since it is unreliable (MRI
# showed little of mouth opening at front of lips) and X29 omitted since it is
# the glottis which is always zero.
if (isNorm == T) { maxArea <- apply(NZE$data[,4:30],1,max) }

for(i in 1:numVTs) {
  j <- i + 1
  while(j <= numVTs) {
    
    # Individual speaker PCAs with normalised area functions
    if (isNorm == T) {
      pca1 <- prcomp(~., data = NZE$data[((i-1)*10 + i):(i*10 + i),4:30]/maxArea[((i-1)*10 + i):(i*10 + i)], scale=T)
      pca2 <- prcomp(~., data = NZE$data[((j-1)*10 + j):(j*10 + j),4:30]/maxArea[((j-1)*10 + j):(j*10 + j)], scale=T) 
    } else {
      pca1 <- prcomp(~., data = NZE$data[((i-1)*10 + i):(i*10 + i),4:30], scale=T)
      pca2 <- prcomp(~., data = NZE$data[((j-1)*10 + j):(j*10 + j),4:30], scale=T) 
    }
    
    # Normality tests
    # shapiro.test(pca1$x[,p])
    # shapiro.test(pca2$x[,p])
    # qqnorm(pca1$x[,p])
    # qqnorm(pca2$x[,p])
    
    # Heteroscedasticity tests
    #lmtest::bptest(pca1$x[,p])
    #car::ncvTest(pca1$x[,p])
    
    # Interspeaker correlations
    cor <- cor.test(pca1$x[,p], pca2$x[,p])
    
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
