# Performing Principal Components Analysis on cross-sectional areas

# N.B. Must run readinMRIdata_oneSet.R first!

# Adapted by Jenny Sahng
# 07/08/2016

# Normalise for maximum area in each shape to eliminate interspeaker effects
# e.g. VT09 shapes much bigger than others.
maxArea=apply(pca[,4:30],1,max)

# Excluding first column X1 because of unreliability of first frame of MRI images (lips are usually poorly defined), and last column X29
# Omit NAs from VT01 hood.txt
pca <- prcomp(~., data=na.omit(pca[,4:30]/maxArea), scale=T)	
summary(pca)

# Plot first two principal components
eplot(pca$x[,1:2], labs=allSpeakers.df[-9,2], centroid=T, formant=T, col=T) # Remove the first 'hood' from VT01
text(pca$x[,1],pca$x[		,2],labels=allSpeakers.df[-9,2])

plot(1:length(pca$sdev),pca$sdev,type="p")

# Add the index of the maximum value for each area
maxAreaIndex=NULL
for(i in 1:length(maxArea))
{
  maxAreaIndex[i]= order(pca[i,4:30],decreasing=T)[2]
}

pca.max<-prcomp(cbind(pca[,4:30]/maxArea,maxAreaIndex),scale=T)

summary(pca.max)

eplot(fdat$x[,1:2],labs=label(daniel.seg),centroid=TRUE,formant=T)
plot(fdat$x[,1],fdat$x[,2],type="n")
text(fdat$x[,1],fdat$x[,2],labels=label(daniel.seg))
