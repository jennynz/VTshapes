# Perform interspeaker correlations in only one region

# Written by Jenny Sahng
# 7/09/2016

source('~/Part IV Project/R code/readAreaFunctions_1Set.R')
spkdata <- read.NZE.data()
allSpeakers.df <- spkdata$data
VTlist <- spkdata$VTlist
numVTs <- spkdata$numVTs
vowelNames <- spkdata$vowelNames

# Principal components to analyse
p.max <- 3

# Normalise?
isNorm <- F

# Oral (o) or pharyngeal (p)?
cavity <- "p"
switch(cavity,
       "o" = {
         cavity.index <- c(4:17)
         cavity <- "oral"
       }, "p" = {
         cavity.index <- -c(1:16,31)
         cavity <- "pharyngeal"
       }
)

# Maximum areas for normalising. X1 is omitted since it is unreliable (MRI
# showed little of mouth opening at front of lips) and X29 omitted since it is
# the glottis which is always zero.
if (isNorm == T) { maxArea <- apply(allSpeakers.df[,4:30],1,max) }
# Normalise by largest oral/pharyngeal area
#if (isNorm == T) { maxArea <- apply(allSpeakers.df[,cavity.index],1,max) }

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
        pca1 <- prcomp(~., data = allSpeakers.df[m, cavity.index]/maxArea[m], scale=T)
        pca2 <- prcomp(~., data = allSpeakers.df[n, cavity.index]/maxArea[n], scale=T) 
      } else {
        pca1 <- prcomp(~., data = allSpeakers.df[m, cavity.index], scale=T)
        pca2 <- prcomp(~., data = allSpeakers.df[n, cavity.index], scale=T) 
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
  write.csv(table, file = paste("corr_inter_", cavity, "_norm.csv", sep=""))
} else {
  write.csv(table, file = paste("corr_inter_", cavity, "_unnorm.csv", sep=""))
}