# Reading in area function data derived from MRI images.

# Area functions (cross-sectional area vs. distance from lips) from VT03, VT05, VT08, VT09, and VT10 are read into R and interpolated with the same number of points as the original matrix. Both sets of each VT are used.

# Adapted by Jenny Sahng
# 15/08/2016



rm(list=ls()) # clear workspace
graphics.off() # close all graphics windows

path<<-"H:\\Documents\\Part IV Project\\Daniel2012\\Data run"

# List of VT name strings from directory names (only VTs 3, 5, 8, 9, 10)
VTList <- list.dirs(path, recursive=FALSE, full.names=FALSE)
numVTs <- length(VTList)

# List of sets (Set1, Set2...)
SetList <- dir(paste(path,VTList[1],sep="\\"))
numSets <- length(SetList)

# List of vowel names (hadd, heed...)
VowelList <- dir(paste(path,VTList[1],SetList[1], "distance_area",sep="\\"))
numVowels <- length(VowelList)
vowelNames <- vector(length=numVowels)
for(i in 1:numVowels) {
  vowelNames[i]<- unlist(strsplit(VowelList[i],"\\."))[1] # Remove .txt
}

compileMRIAreas<-function(spk,interpN=FALSE)
{
  # Can decide how many values to interpolate over, or leave it with the default which is 29 (the number of data points in the area functions/numrows in .txt files)
  
  # Make data matrix and vowel names vector
  alldat <- matrix(nrow=numVowels*numSets,ncol=29,byrow=T)
  
  # for each speaker, read area function data for each vowel in each set, interpolating the cross-sectional areas
  for (i in 1:numVowels) {
    for(j in 1:numSets) {
      filepath <- paste(path,spk,SetList[j],"distance_area",VowelList[i],sep="\\")
      datfile <- read.table(filepath)
      
      if (interpN) { n <- interpN }
      else { n <- nrow(datfile) }
      
      # Need to linearly interpolate data, as different distance step in oral region than pharyngeal region.
      LinDatfil <- approx(datfile[,1],datfile[,2],n=n)
      alldat[(i-1)*2+j,] <- LinDatfil$y
    }
  }
  
  # Create data frame with speaker labels, set labels, vowel labels, and cross-sectional areas. 
  alldat.df=data.frame(spk = factor(rep(spk,numVowels*numSets)), set = factor(rep(SetList,numVowels)), vow = factor(rep(vowelNames, each=2)), alldat)
  return(alldat.df)
}

allSpeakers.df <- NULL

for (i in 1:numVTs)
{
  allVowels.df <- compileMRIAreas(spk=VTList[i])
  allSpeakers.df <- rbind(allSpeakers.df, allVowels.df)
}

# Omit VT01 hood.txt since they're all NAs.
allSpeakers.df = allSpeakers.df
