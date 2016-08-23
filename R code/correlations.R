## Inter-speaker and Intra-speaker Corre#ations

# Performs Pearson's product-moment correlations between the first and second
# principal components of each speaker with all other speakers, and then between
# the two sets of each speaker. Before running this script, please run
# readinMRIdata_oneSet.R and pcaAreaFunctions.R

# Adapted by Jenny Sahng
# 16/08/2016


# Inter-speaker Correlations
vt1pca <- prcomp(~., data=na.omit(allSpeakers.df[1:11,4:30]/maxArea), scale=T)
vt2pca <- prcomp(~., data=na.omit(allSpeakers.df[12:22,4:30]/maxArea), scale=T)
cor.test(vt1pca$rotation[,1], vt2pca$rotation[,1])

plot(c(0,28),c(-0.4,0.4),type="n", xlab="Data point", ylab="Cross-sectional area (mm^2)")
lines(vt1pca$rotation[,1], col="red")
lines(vt2pca$rotation[,1], col="blue")
title(main = "PC1 for VT01 and VT02 area function data points")
legend(0, -0.3, c("VT01","VT02"), lty=c(1,1), col=c("red","blue"))