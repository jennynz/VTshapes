## Individual Speaker Variances

# Written by Jenny Sahng
# 12/09/2016

source('~/Part IV Project/R code/readAreaFunctions_1Set.R', echo=TRUE)

# Number of principal components to test
np <- 3

vars <- matrix(data = NA, nrow = numVTs, ncol = np, byrow = TRUE)
colnames(vars) <- 1:np
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
    vars[i,] <- unname(summary(pca)$importance[2,1:np])
    
  }
}

# Write table to file
if (isNorm == T) {
  write.csv(vars, file = paste("vars_normalised.csv",sep=""))
} else {
  write.csv(vars, file = paste("vars_unnormalised.csv",sep=""))
}
