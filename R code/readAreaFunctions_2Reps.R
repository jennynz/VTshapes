# Reading 2 repetitions of the same area functions for a speaker

# Quantifying the variability between each markup of MRI images. One speaker and
# one set (the same set) only.

# Adapted by Jenny Sahng
# 12/09/2016

rm(list=ls()) # clear workspace
graphics.off() # close all graphics windows
setwd("~/Part IV Project/R code")

path<<-"H:\\Documents\\Part IV Project\\All VT data\\VT04\\Set1\\distance_area"

VTname <- "VT04"

# List of vowel names (hadd, heed...) including repetitions
areaFiles <- dir(path)[c(1,2,5,6,10,11,14,15)]
numVowels <- length(areaFiles)
vowelNames <- vector(length=numVowels)
for(i in 1:numVowels) {
  vowelNames[i] <- unlist(strsplit(areaFiles[i],"\\."))[1] # Remove .txt
}

compileMRIAreas<-function(interpN=FALSE)
{
  # Can decide how many values to interpolate over, or leave it with the default which is 29 (the number of data points in the area functions/numrows in .txt files)
  
  # Make data matrix and vowel names vector
  alldat <- matrix(nrow=numVowels,ncol=29,byrow=T)
  
  # Read area function data for each vowel in each set, interpolating the cross-sectional areas
  for(i in 1:numVowels)
  {
    filepath <- paste(path,areaFiles[i],sep="\\")
    datfile <- read.table(filepath)
    
    if (interpN) { n <- interpN }
    else { n <- nrow(datfile) }
    
    # Need to linearly interpolate data, as different distance step in oral region than pharyngeal region.
    LinDatfil <- approx(datfile[,1],datfile[,2],n=n)
    alldat[i,] <- LinDatfil$y
  }
  
  # Create data frame with speaker labels, vowel labels, and cross-sectional areas. 
  alldat.df=data.frame(vow=factor(vowelNames),alldat)
  return(alldat.df)
}

allSpeakers.df <- compileMRIAreas
