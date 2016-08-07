# Performing Principal Components Analysis on cross-sectional areas

# N.B. Must run readinMRIdata_oneSet.R first!

# Adapted by Jenny Sahng
# 07/08/2016

# Normalise for maximum area in each shape to eliminate interspeaker effects
# e.g. VT09 shapes much bigger than others.
maxArea=apply(allSpeakers.df[,4:30],1,max)

# Excluding first column X1 because of unreliability of first frame of MRI images (lips are usually poorly defined), and last column X29
# Omit NAs from VT01 hood.txt
pca <- prcomp(~., data=na.omit(allSpeakers.df/maxArea), scale=T)	
pca.summ <- summary(pca)

# Plot first two principal components
text(pca$x[,1],pca$x[,2],labels=allSpeakers.df[-9,2])

# Centroids only
eplot(pca$x[,1:2], labs=allSpeakers.df[-9,2], centroid=T, formant=T, col=T) # Remove the first 'hood' from VT01

# Plot of standard deviations 
plot(1:length(pca$sdev),pca$sdev,type="p")

# Plot of proportions of variance
pca.var <- pca.summ$importance[2,]
plot(1:length(x$importance[2,]),x$importance[2,], type="p")

# Add the index of the maximum value for each area
maxAreaIndex=NULL
for(i in 1:length(maxArea))
{
  maxAreaIndex[i]= order(allSpeakers.df[i,4:30],decreasing=T)[2]
}

pca.max<-prcomp(~., data=cbind(na.omit(allSpeakers.df[,4:30]/maxArea),maxAreaIndex[-9]),scale=T)

summary(pca.max)

eplot(pca.max$x[,1:2],labs=label(daniel.seg),centroid=TRUE,formant=T)
text(pca.max$x[,1],pca.max$x[,2],labels=label(daniel.seg))
