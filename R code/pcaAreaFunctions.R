# Performing Principal Components Analysis on cross-sectional areas

# N.B. Must run readinMRIdata_oneSet.R first!

# Adapted by Jenny Sahng
# 07/08/2016

## need to normalise for maximum area in each shape - other wize you get speaker effects - Vt09 shapes much bigger than every one elses.
maxArea=apply(pca[,4:30],1,max)

# Principal Components Analysis
# Excluding first column X1 because of unreliability of first frame of MRI images (lips are usually poorly defined), and last column X29
# Omit NAs from VT01 hood.txt
pca <- prcomp(~., data=na.omit(pca[,4:30]/maxArea), scale=T)	
#pca <- prcomp(pca[,4:30]/maxArea,scale=T)	
summary(pca)

eplot(pca$x[,1:2],labs=pca[,3],centroid=TRUE,formant=T)
plot(pca$x[,1],pca$x[,2],type="n")
text(pca$x[,1],pca$x[		,2],labels=pca[,3])

plot(1:length(pca$sdev),pca$sdev,type="p")

## Now going to add the index of the maximum value for each area

maxAreaIndex=NULL
for(i in 1:length(maxArea))
{
  maxAreaIndex[i]= order(pca[i,4:30],decreasing=T)[2]
}

foo5<-prcomp(cbind(pca[,4:30]/maxArea,maxAreaIndex),scale=T)

summary(foo5)

eplot(fdat$x[,1:2],labs=label(daniel.seg),centroid=TRUE,formant=T)
plot(fdat$x[,1],fdat$x[,2],type="n")
text(fdat$x[,1],fdat$x[,2],labels=label(daniel.seg))
