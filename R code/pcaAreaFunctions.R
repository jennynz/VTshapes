# Performing Principal Components Analysis on cross-sectional areas

# N.B. Must run readinMRIdata_oneSet.R first!

# Adapted by Jenny Sahng
# 07/08/2016

# Normalise for maximum area in each shape to eliminate interspeaker effects
# e.g. VT09 shapes much bigger than others.
maxArea=apply(allSpeakers.df[,4:30],1,max)

# Excluding first column X1 because of unreliability of first frame of MRI images (lips are usually poorly defined), and last column X29
pca <- prcomp(~., data=na.omit(allSpeakers.df[,4:30]/maxArea), scale=T)	
pca.summ <- summary(pca)

# Centroids only
plot.new()
eplot(pca$x[,1:2], labs=allSpeakers.df[,2], centroid=T, formant=T, col=T)

# Plot first two principal components
plot.new()
text(pca$x[,1],pca$x[,2],labels=allSpeakers.df[,2])

# Plot of standard deviations 
plot(1:length(pca$sdev),pca$sdev,type="p")

# Plot of proportions of variance
pca.var <- pca.summ$importance[2,]
plot(1:length(pca.var),pca.var, type="p")

# Add the index of the maximum value for each area
maxAreaIndex=NULL
for(i in 1:length(maxArea))
{
  maxAreaIndex[i]= order(allSpeakers.df[i,4:30],decreasing=T)[2]
}

pca.max<-prcomp(~., data=cbind(na.omit(allSpeakers.df[,4:30]/maxArea),maxAreaIndex),scale=T)

summary(pca.max)

eplot(pca.max$x[,1:2],labs=allSpeakers.df[,2],centroid=TRUE,formant=T)
text(pca.max$x[,1],pca.max$x[,2],labels=allSpeakers.df[,2])
