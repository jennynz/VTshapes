# Reading 2 sets of area functions per speaker

# Both sets of area functions (cross-sectional area vs. distance from lips) are
# read into R and interpolated with the same number of points as the original
# matrix. Specific to the current file structure in project directory.
# VTs with two sets of area function data are VT03, VT05, VT08, VT09, VT10, VT11.

# Adapted by Jenny Sahng
# 29/08/2016

rm(list=ls()) # clear workspace
graphics.off() # close all graphics windows
setwd("~/Part IV Project/R code")

path<<-"H:\\Documents\\Part IV Project\\All VT data"

# List of VT name strings from directory names (only VTs 3, 5, 8, 9, 10, 11)
VTlist <- list.dirs(path, recursive=FALSE, full.names=FALSE)[c(3,5,8,9,10,11)]
numVTs <- length(VTlist)

# List of sets (Set1, Set2...)
SetList <- list.dirs(paste(path,VTlist[1],sep="\\"), recursive=FALSE, full.names=FALSE)
numSets <- length(SetList)

# List of vowel names (hadd, heed...)
areaFiles <- dir(paste(path,VTlist[1],SetList[1], "distance_area",sep="\\"))
numVowels <- length(areaFiles)
vowelNames <- vector(length=numVowels)
for(i in 1:numVowels) {
  vowelNames[i] <- unlist(strsplit(areaFiles[i],"\\."))[1] # Remove .txt
}

compileMRIAreas<-function(spk,interpN=FALSE)
{
  # Can decide how many values to interpolate over, or leave it with the default which is 29 (the number of data points in the area functions/numrows in .txt files)
  
  # Make data matrix and vowel names vector
  if (interpN) { n <- interpN }
  else { n <- nrow(datfile) }
  alldat <- matrix(nrow=numVowels*numSets,ncol=n,byrow=T)
  
  # for each speaker, read area function data for each vowel in each set, interpolating the cross-sectional areas
  for (i in 1:numVowels) {
    for(j in 1:numSets) {
      filepath <- paste(path,spk,SetList[j],"distance_area",areaFiles[i],sep="\\")
      datfile <- read.table(filepath)
      
      # Need to linearly interpolate data, as different distance step in oral region than pharyngeal region.
      LinDatfil <- approx(datfile[,1],datfile[,2],n=n)
      alldat[(i-1)*2+j,] <- LinDatfil$y
    }
  }
  
  # Create data frame with speaker labels, set labels, vowel labels, and cross-sectional areas. 
  alldat.df <- data.frame(spk = factor(rep(spk,numVowels*numSets)), set = factor(rep(SetList,numVowels)), vow = factor(rep(vowelNames, each=2)), alldat)
  return(alldat.df)
}

allSpeakers.df <- NULL

for (i in 1:numVTs)
{
  allVowels.df <- compileMRIAreas(spk=VTlist[i])
  allSpeakers.df <- rbind(allSpeakers.df, allVowels.df)
}
