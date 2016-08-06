## Oct 16th- doing principle components on cross-sectional areas

## need to normalise for maximum area in each shape - other wize you get speaker effects - Vt09 shapes much bigger than every one elses.
maxArea=apply(allSpeakers.df[,4:31],1,max)

# Principal Components Analysis
# Excluding first column X1 because of unreliability of first frame of MRI images (lips are usually poorly defined), and last column X29
# Omit NAs from VT01 hood.txt
pca <- prcomp(~., data=na.omit(allSpeakers.df[,4:30]/maxArea), scale=T)	
#pca <- prcomp(allSpeakers.df[,4:30]/maxArea,scale=T)	

eplot(allSpeakers.df$x[,1:2],labs=allMRI.df[,3],centroid=TRUE,formant=T)
plot(allSpeakers.df$x[,1],allSpeakers.df$x[,2],type="n")
text(allSpeakers.df$x[,1],allSpeakers.df$x[		,2],labels=allMRI.df[,3])

plot(1:length(allSpeakers.df$sdev),allSpeakers.df$sdev,type="p")

eplot(foo4$x[,1:2],labs=allMRI.df[,3],centroid=TRUE,formant=T)
plot(foo4$x[,1],foo4$x[,2],type="n")
text(foo4$x[,1],foo4$x[		,2],labels=allMRIn.df[,3])


## Now going to add the index of the maximum value for each area

maxAreaIndex=NULL
for(i in 1:length(maxArea))
{
maxAreaIndex[i]= order(allMRI.df[i,4:31],decreasing=T)[2]
}


> foo5<-prcomp(cbind(allMRI.df[,4:31]/maxArea,maxAreaIndex),scale=T)

> summary(foo5)
Importance of components:
                          PC1    PC2    PC3     PC4     PC5     PC6     PC7    PC8     PC9   PC10    PC11   PC12    PC13    PC14    PC15    PC16   PC17
Standard deviation     3.3818 2.4162 1.7476 1.52615 1.29727 1.03905 0.97763 0.6769 0.63331 0.5899 0.50062 0.4695 0.41205 0.35855 0.33354 0.31714 0.2744
Proportion of Variance 0.3944 0.2013 0.1053 0.08031 0.05803 0.03723 0.03296 0.0158 0.01383 0.0120 0.00864 0.0076 0.00585 0.00443 0.00384 0.00347 0.0026
Cumulative Proportion  0.3944 0.5957 0.7010 0.78129 0.83932 0.87655 0.90951 0.9253 0.93914 0.9511 0.95978 0.9674 0.97324 0.97767 0.98150 0.98497 0.9876
                          PC18    PC19    PC20    PC21    PC22    PC23    PC24   PC25    PC26    PC27    PC28    PC29
Standard deviation     0.25522 0.23631 0.21852 0.20654 0.18290 0.16376 0.15998 0.1428 0.13456 0.11020 0.08232 0.07645
Proportion of Variance 0.00225 0.00193 0.00165 0.00147 0.00115 0.00092 0.00088 0.0007 0.00062 0.00042 0.00023 0.00020
Cumulative Proportion  0.98981 0.99174 0.99339 0.99486 0.99601 0.99694 0.99782 0.9985 0.99915 0.99956 0.99980 1.00000
	
eplot(foo5$x[,1:2],labs=allMRI.df[,3],centroid=TRUE,formant=T)
plot(foo5$x[,1],foo5$x[,2],type="n")
text(foo5$x[,1],foo5$x[		,2],labels=allMRIn.df[,3])




eplot(fdat$x[,1:2],labs=label(daniel.seg),centroid=TRUE,formant=T)
plot(fdat$x[,1],fdat$x[,2],type="n")
text(fdat$x[,1],fdat$x[,2],labels=label(daniel.seg))









