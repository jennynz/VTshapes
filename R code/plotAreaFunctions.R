# Plotting normalised area functions

# Area functions are compared within speakers for different vowels, and across speakers for all vowels, as well as subgroups such as high, low, front and back.
# Specific to the current file structure in project directory.
# Assumes one set only. See the original file plottingDanielsMRIdata.txt to use both sets.

# Adapted by Jenny Sahng
# 07/09/2016


rm(list=ls()) # clear workspace
graphics.off() # close all graphics windows

path<<-"H:\\Documents\\Part IV Project\\All VT data"

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

# Colour lists
cols.warm <- c("firebrick4", "chocolate4", "darkgoldenrod", "chartreuse4")
cols.cold <- c("aquamarine4", "darkcyan", "deepskyblue4", "darkslateblue", "darkorchid4")
cols.dark <- c("maroon", "deeppink4", "indianred4", "gray4")
cols.all <- c(cols.warm, cols.cold, cols.dark)
cols.all <- rainbow(numVowels, s = 1, v = 1, start = 0.7,
                    end = max(1, numVowels - 1)/numVowels, alpha = 1)

"maxVTvals"<-function(spk)
{
  Dirpath <- paste(path,spk,"Set1","distance_area",sep="\\")
  filesInDir <- dir(Dirpath)
  alldat <- NULL
  for(i in 1:length(filesInDir))
  {
    filepath=paste(Dirpath,filesInDir[i],sep="\\")
    datfile=read.table(filepath)
    alldat=rbind(alldat,datfile)
  }
  return(apply(alldat,2,max))
}

"makeplot"<-function(spk, vow="had", col="red", xlim=c(0,180), ylim=c(0,250))
{
  fullpath <- paste(path,spk,"Set1","distance_area",paste(vow,"txt",sep="."),sep="\\")
  datfile <- read.table(fullpath)
  plot(datfile[,1], datfile[,2], type="l", col = col,
       xlim = xlim, ylim = ylim, xlab = "Distance from lips (mm)",
       ylab = expression(Cross-sectional ~ area ~ (cm^{2})))
}

"plotMRI"<-function(spk, vowels="All", xlim=c(0,175), ylim=c(0,510))
{
  switch(vowels, 
    "All" = {indices <- 1:numVowels},
    "Front" = {indices <- c(1,2,3,4)},
    "Back" = {indices <- c(7,8,10,11)},
    "Central" = {indices <- c(5,6,9)},
    "High" = {indices <- c(4,6,11)},
    "Low" = {indices <- c(1,2,3,8)},
    "Mid" = {indices <- c(5,7,9,10)}
  )
  for (i in indices) {
    makeplot(vow=vowelNames[i], col=cols.all[i], xlim=xlim, ylim=ylim, spk=spk)
    par(new=T)  
  }
  title(paste(vowels, " vowels for ", spk))
}

## Maximum vocal tract values (not sure if this is needed)
for (v in VTlist) {
  maxVTvals(spk=v)
}

# Optimal axis limits
x <- c(200, 200, 200, 175, 155, 190, 190, 160, 205, 190, 190, 195)
y <- c(800, 700, 550, 550, 550, 550, 550, 650, 950, 800, 700, 1200)

## Plot vowels
selection <- c("All", "Front", "Central", "Back", "High", "Mid", "Low")
for (j in selection) {
  par(mfrow=c(3,4), lwd=1, cex=0.8)
  for (i in 1:numVTs) {
    plotMRI(spk = VTlist[i], vowels = "High", xlim = c(0,200), ylim = c(0,y[i]))
  }
}
