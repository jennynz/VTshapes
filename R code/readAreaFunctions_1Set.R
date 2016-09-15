# Reading 1 set of area functions per speaker

# Area functions (cross-sectional area vs. distance from lips)are read into R
# and interpolated with the same number of points as the original matrix. 
# Specific to the current file structure in project directory. Reads Set 1 only.

# Adapted by Jenny Sahng
# 05/08/2016

"compileMRIAreas" <- function(spk, interpN = FALSE, vowelNames)
{
  # Can decide how many values to interpolate over, or leave it with the default which is 29 (the number of data points in the area functions/numrows in .txt files)
  
  # Make data matrix and vowel names vector
  if (interpN) { n <- interpN }
  else { n <- nrow(read.table(paste(path,spk,"Set1","distance_area",areaFiles[1],sep="\\"))) }
  alldat <- matrix(nrow=numVowels,ncol=n,byrow=T)
  
  # for each speaker, go into their directory, reading the vocal tract data for each vowel, interpolate the cross-sectional areas
  #use file name to determine vowel name, write the cross-sectional areas - only to data matrix
  for(i in 1:numVowels)
  {
    filepath <- paste(path,spk,"Set1","distance_area",areaFiles[i],sep="\\")
    datfile <- read.table(filepath)
    
    # Need to linearly interpolate data, as different distance step in oral region than pharyngeal region.
    LinDatfil <- approx(datfile[,1],datfile[,2],n=n)
    alldat[i,] <- LinDatfil$y
  }
  
  # Create data frame with speaker labels, vowel labels, and cross-sectional areas. 
  alldat.df=data.frame(spk=factor(rep(spk,numVowels)),vow=factor(vowelNames),alldat)
  return(alldat.df)
}

"read.NZE.data" <- function(interpN = FALSE) {
  
  setwd("~/Part IV Project/R code")
  
  path <<- "H:\\Documents\\Part IV Project\\All VT data"
  
  # List of VT name strings (VT01, VT02...)
  VTlist <- list.dirs(path, recursive=FALSE, full.names=FALSE)
  numVTs <- length(VTlist)
  
  # List of vowel names (hadd, heed...)
  areaFiles <<- dir(paste(path,VTlist[1],"Set1","distance_area",sep="\\"))
  numVowels <<- length(areaFiles)
  vowelNames <- vector(length=numVowels)
  for(i in 1:numVowels) {
    vowelNames[i] <- unlist(strsplit(areaFiles[i],"\\."))[1] # Remove .txt
  }
  
  allSpeakers.df <- NULL
  
  for (i in 1:numVTs)
  {
    allVowels.df <- compileMRIAreas(spk=VTlist[i], interpN = interpN, vowelNames = vowelNames)
    allSpeakers.df <- rbind(allSpeakers.df, allVowels.df)
  }
  
  output <- list("data" = allSpeakers.df, "numVTs" = numVTs, "VTlist" = VTlist, "vowelNames" = vowelNames)
  return(output)
  
}
