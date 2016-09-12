## Individual Speaker Variances

# Written by Jenny Sahng
# 12/09/2016

source('~/Part IV Project/R code/readAreaFunctions_1Set.R', echo=TRUE)

# Number of principal components to test
np <- 3

vars <- matrix(data = NA, nrow = numVTs, ncol = np+1, byrow = TRUE)
colnames(vars) <- c("PC1+PC2", paste("PC", 1:np, sep=""))
rownames(vars) <- VTlist

# Normalise?
isNorm <- T

# Maximum areas for normalising. X1 is omitted since it is unreliable (MRI
# showed little of mouth opening at front of lips) and X29 omitted since it is
# the glottis which is always zero.
if (isNorm == T) { maxArea <- apply(allSpeakers.df[,4:30],1,max) }

for (p in 1:np) {
  for(i in 1:numVTs) {
      
    # Individual speaker PCAs with normalised area functions
    if (isNorm == T) {
      pca <- prcomp(~., data = allSpeakers.df[((i-1)*10 + i):(i*10 + i),4:30]/maxArea[((i-1)*10 + i):(i*10 + i)], scale=T)
    } else {
      pca1 <- prcomp(~., data = allSpeakers.df[((i-1)*10 + i):(i*10 + i),4:30], scale=T)
    }

    # Write to tables
    vars[i,1] <- summary(pca)$importance[2,1] + summary(pca)$importance[2,2]
    vars[i,-1] <- unname(summary(pca)$importance[2,1:np])
    
  }
}

# Write table to file
if (isNorm == T) {
  write.csv(vars, file = paste("vars_normalised.csv",sep=""))
} else {
  write.csv(vars, file = paste("vars_unnormalised.csv",sep=""))
}

# Plot as series bar graph
barcols <- c("sienna1", "slategray1","slategray3","slategray4")
barplot(t(vars), beside=T, type="n",col=barcols)
grid(nx=1, ny=18, col="gray80",lty=1)
par(new=T)
barplot(t(vars), beside=T, legend.text=TRUE, ylab="% Variance", ylim=c(0,0.9), main="Variance accounted for by principal components", args.legend = list(x = "topleft", bty="n", ncol=np), col=barcols)
