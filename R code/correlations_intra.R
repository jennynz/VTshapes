## Inter-speaker and Intra-speaker Correlations

# Performs Pearson's product-moment correlations between the first and second
# principal components of each speaker with all other speakers, and then between
# the two sets of each speaker.
# Run readAreaFunctions_5VT2Set.R and pca_5VT2Set_combined.R first

# Written by Jenny Sahng
# 26/08/2016

source('~/Part IV Project/R code/readAreaFunctions_5VT2Set.R', echo=TRUE)
source('~/Part IV Project/R code/pca_5VT2Set_combined.R', echo=TRUE)

## Inter-speaker correlations

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