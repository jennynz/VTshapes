## Intra-speaker Correlations

# Performs Pearson's product-moment correlations between principal component #p 
# of each set within each speakers (the ones which have two datasets available
# and analysed). When changing the principal component number to compare, make
# sure to change the name of the file that the tables are written to as well.

# Written by Jenny Sahng
# 26/08/2016

source('~/Part IV Project/R code/readAreaFunctions_2Set.R', echo=TRUE)
source('~/Part IV Project/R code/pca_2Set_combined.R', echo=TRUE)

corr <- matrix(data = NA, nrow = numVTs-1, ncol = numVTs-1, byrow = TRUE)
colnames(corr) <- VTlist[-1]
rownames(corr) <- VTlist[-numVTs]

pvalues <- matrix(data = NA, nrow = numVTs-1, ncol = numVTs-1, byrow = TRUE)
colnames(pvalues) <- VTlist[-1]
rownames(pvalues) <- VTlist[-numVTs]

for(i in 1:numVTs) {
  j <- i + 1
  while(j <= numVTs) {
    
    # Individual speaker PCAs
    pca1 <- prcomp(~., data=na.omit(allSpeakers.df[((i-1)*10 + i):(i*10 + i),4:30]/maxArea), scale=T)
    pca2 <- prcomp(~., data=na.omit(allSpeakers.df[((j-1)*10 + j):(j*10 + j),4:30]/maxArea), scale=T)
    
    # Interspeaker correlations
    cor <- cor.test(pca1$rotation[,1], pca2$rotation[,1])
    
    # Plots of two area functions
    # plot(c(0,28),c(-0.4,0.4),type="n", xlab="Data point", ylab="Cross-sectional area (mm^2)")
    # lines(pca1$rotation[,1], col="red")
    # lines(pca2$rotation[,1], col="blue")
    # title(main = "PC1 for VT01 and VT02 area function data points")
    # legend(0, -0.3, c("VT01","VT02"), lty=c(1,1), col=c("red","blue"))
    
    cat("\n\t VT", i, " and VT", j, 
        "\n\t Correlation: ", cor$estimate,
        "\n\t P-value: ", cor$p.value,
        "\n", sep="")
    
    j <- j + 1
  }
}