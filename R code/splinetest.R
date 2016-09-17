library(graphics)
library(splines) 

# Prepare data
path = "H:\\Documents\\Part IV Project\\All VT data"

# List of VT name strings (VT01, VT02...)
VTlist <- list.dirs(path, recursive=FALSE, full.names=FALSE)
numVTs <- length(VTlist)

# List of vowel names (hadd, heed...)
areaFiles <- dir(paste(path,VTlist[1],"Set1","distance_area",sep="\\"))
numVowels <- length(areaFiles)
vowelNames <- vector(length=numVowels)
for(i in 1:numVowels) {
  vowelNames[i] <- unlist(strsplit(areaFiles[i],"\\."))[1] # Remove .txt
}

interpN <- 44
spk <- "VT01"
i <- 4

filepath <- paste(path,spk,"Set1","distance_area",areaFiles[i],sep="\\")
datfile <- read.table(filepath)

# Need to linearly interpolate data, as different distance step in oral region than pharyngeal region.
LinDatfil <- approx(datfile[,1],datfile[,2],n=interpN)

x <- LinDatfil$x
y <- LinDatfil$y

## Linear Model
fit = lm(y ~ x )

## Polynomial
fit.3 = lm(y ~ poly(x,3) )

## Natural Spline
fit.ns.3 = lm(y ~ ns(x, 3) )

## Smoothing Spline
fit.sp = smooth.spline(y ~ x, nknots=15)

## Plot them all!
plot(x,y, xlim=c(min(x),max(x)), ylim=c(min(y),max(y)), pch=16, cex=.5,
     ylab = "Cross-sectional area", xlab = "Distance from lips", main = "Comparison of models", sub = "Splines")

## Linear
lines(x, predict(fit, data.frame(x=x)), col='orange', lty=3)

## Polynomial
lines(x, predict(fit.3, data.frame(x=x)), col='light blue', lty=4)

## Natural Spline
lines(x, predict(fit.ns.3, data.frame(x=x)), col='green', lty=5)

## Smoothing Spline
lines(fit.sp, col='blue', lty=6)

## Kernel Curve
lines(ksmooth(x, y, "normal", bandwidth = 5), col = 'purple', lty=7)
legend("topright", c("Linear","Polynomial","Natural Spline","Smoothing Spline","Kernel"),
       col=c('black','light blue','green','blue','purple'), lty=c(3,4,5,6,7), lwd=2)

## Okay so smoothing splines are awesome. How many nknots?

plot(x,y, xlim=c(min(x),max(x)), ylim=c(min(y),max(y)), pch=16, cex=.5,
     ylab = "Cross-sectional area", xlab = "Distance from lips", 
     main = "Comparison of number of knots used for splines")

n <- seq(10,20,2)
colorlist <- c('red','green','blue','purple',"hotpink","orange")
linetypes <- c(1,2,3,4,5,6)
for (i in 1:length(n)) {
  fit.sp.n = smooth.spline(y ~ x, nknots=n[i])
  lines(fit.sp.n, col=colorlist[i], lty=linetypes[i])
}

legend("topright", c("Linear","10","12","14","16","18","20"),
       col=c('black',colorlist), lty=c(2, linetypes), lwd=2)