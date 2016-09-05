# Effect of one VT on dataset

# Runs PCA on dataset with one VT excluded to see its effect on the combined dataset.

# Written by Jenny Sahng
# 06/09/2016

source('~/Part IV Project/R code/readAreaFunctions_1Set.R', echo=TRUE)

# IPA symbols and custom colours for eplot. Paste in vowels from file 'IPAvowels'
levels(allSpeakers.df$vow) <- c()

.libPaths('H:/Documents/Rlibraries')
library('emuR')

# Normalise for maximum area in each shape to eliminate interspeaker effects 
# Excluding first column X1 because of unreliability of first frame of MRI
# images (lips are usually poorly defined), and last column X29 e.g. VT09 shapes
# much bigger than others.
maxArea <- apply(allSpeakers.df[,4:30],1,max)

# Initialise table
variances <- matrix(data = NA, nrow = (numVTs+1), ncol = 5, byrow = TRUE)
rownames(variances) <- c("All 12 VTs", paste("exclude", VTlist, sep=" "))
colnames(variances) <- c("PC1", "PC2", "PC3", "PC1+PC2", "Variance in PC1")

# Combined dataset of all 12 VTs
pca <- prcomp(~., data = allSpeakers.df[,4:30]/maxArea, scale=T)	
pca.var <- summary(pca)$importance[2,]
variances[1,1:4] <- unname(c(pca.var[1:3], pca.var[1] + pca.var[2]))
variances[1,5] <- var(pca$x[,1])

# Variances when one VT is excluded from the combined dataset, one at a time
for (i in 1:numVTs) {
  excludedVT <- ((i-1)*10+i) : ((i*10)+i)
  pca <- prcomp(~., data = allSpeakers.df[-excludedVT,4:30]/maxArea[-excludedVT], scale=T)	
  pca.var <- summary(pca)$importance[2,]
  variances[i+1,1:4] <- unname(c(pca.var[1:3], pca.var[1] + pca.var[2]))
  variances[i+1,5] <- var(pca$x[,1])
}

# Write to csv
write.csv(variances, file = "variances_excludeVT.csv")
