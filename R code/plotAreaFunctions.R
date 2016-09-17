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
cols.warm <- c("red", "maroon", "orange", "yellow")
cols.cold <- c("green1", "green4", "aquamarine3", "aquamarine1", "blue4")
cols.dark <- c("maroon", "blue4", "blue1", "purple")
cols.all <- c(cols.warm, cols.cold, cols.dark)

"maxVTvals"<-function(spk)
{
  Dirpath=paste(path,spk,"Set1","distance_area",sep="\\")
  filesInDir=dir(Dirpath)
  alldat=NULL
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
  fullpath=paste(path,spk,"Set1","distance_area",paste(vow,"txt",sep="."),sep="\\")
  datfile=read.table(fullpath)
  plot(datfile[,1],datfile[,2],type="l",col=col,xlim=xlim,ylim=ylim,xlab="distance from lips",ylab="cross-sectional area")
}

"plotMRI"<-function(spk, vowels="All", xlim=c(0,175), ylim=c(0,510))
{
  switch(vowels, 
    "All" = {indices <- 1:numVowels},
    "Front" = {indices <- c(1,3,4,5)},
    "Back" = {indices <- c(2, 7, 8, 9, 10)},
    "Mid" = {indices <- c(5, 6, 9, 11)},
    "High" = {indices <- c(4, 7, 11)},
    "Central" = {indices <- c(1, 3, 5, 6, 10)}
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
x <- c(190, 190, 175, 175, 155, 190, 190, 160, 205, 190, 190, 195)
y <- c(650, 650, 510, 510, 400, 650, 650, 640, 780, 650, 650, 890)

## Plot vowels
selection <- c("All", "Front", "Back", "Mid", "High", "Central")
for (j in selection) {
  par(mfrow=c(3,4),lwd=1)
  for (i in 1:numVTs) {
    plotMRI(spk = VTlist[i], vowels = j, xlim = c(0,x[i]), ylim = c(0,y[i]))
  }
}
