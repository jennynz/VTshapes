## Variance and correlation analyses for quantifying variability in methods

# Analyses between repetitions of the same vowels for the same speaker to 
# quantify variability in manually marking up MRI images. Currently hard-coded
# (in the for loops) for only two repetitions, but may be generalised if time
# allows.

# Written by Jenny Sahng
# 13/09/2016

source('~/Part IV Project/R code/readAreaFunctions_2Reps.R', echo=TRUE)

# Assumes 1 VT but could modify to accommodate more.

# Number of repetitions. Currently assumes 2.
nr <- 2

# Number of principal components to test
np <- 3

# Table for variances
vars <- matrix(data = NA, nrow = nr, ncol = np+1, byrow = TRUE)
colnames(vars) <- c("PC1+PC2", paste("PC", 1:np, sep=""))
rownames(vars) <- paste("Rep", 1:nr, sep=" ")

# Table for correlations
corr <- matrix(data = NA, nrow = np, ncol = 2, byrow = TRUE)
colnames(corr) <- c("Correlation", "P-value")
rownames(corr) <- paste("PC", 1:np, sep="")

# Normalise?
isNorm <- T

# Maximum areas for normalising. X1 is omitted since it is unreliable (MRI
# showed little of mouth opening at front of lips) and X29 omitted since it is
# the glottis which is always zero.
if (isNorm == T) {
  maxArea <- apply(allSpeakers.df[,3:29],1,max)
  allSpeakers.df[,3:29]/maxArea
}

corrdata <- list()

for (r in 1:nr) {
  
  # PCA on each repetition as a group  
  pca <- prcomp(~., data = allSpeakers.df[seq(r,numVowels,nr),3:29], scale=T)
  
  # Variances
  vars[r,1] <- summary(pca)$importance[2,1] + summary(pca)$importance[2,2]
  vars[r,-1] <- unname(summary(pca)$importance[2,1:np])
  
  # Save data for correlations
  corrdata[r] <- list(pca$x)

}

# Correlations between all PCs
for (p in 1:np) {
  
  # Hard-coded r = 2
  cor <- cor.test(corrdata[[1]][,p], corrdata[[2]][,p])
  
  # Write to tables
  corr[p,1] <- unname(cor$estimate)
  corr[p,2] <- unname(cor$p.value)
}
