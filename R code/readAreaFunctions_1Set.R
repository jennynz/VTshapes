# Reading 1 set of area functions per speaker

# Area functions (cross-sectional area vs. distance from lips)are read into R
# and interpolated with the same number of points as the original matrix. 
# Specific to the current file structure in project directory. Reads Set 1 only.

# Adapted by Jenny Sahng
# 05/08/2016

"compileMRIAreas" <- function(areaFiles = areaFiles, numVowels = numVowels,
                              vowelNames = vowelNames, VTlist = VTlist, path,
                              spk, VTlengths, interpN = FALSE, smooth = TRUE)
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
    VTlengths <- c(VTlengths, datfile[dim(datfile)[1],1])
    
    # Need to linearly interpolate data, as different distance step in oral region than pharyngeal region.
    LinDatfil <- approx(datfile[,1],datfile[,2],n=n)
    x <- LinDatfil$x
    y <- LinDatfil$y
    
    # Fit a smoothing spline to the area function
    if (smooth) {y <- smooth.spline(y ~ x, nknots=15)$y }
    
    alldat[i,] <- y
    
  }
  
  # Create data frame with speaker labels, vowel labels, and cross-sectional areas. 
  alldat.df=data.frame(spk=factor(rep(spk,numVowels)),vow=factor(vowelNames),alldat)
  return(list("areas" = alldat.df, "lengths" = VTlengths))
}

"read.NZE.data" <- function(path = "H:\\Documents\\Part IV Project\\All VT data", interpN = FALSE, smooth = FALSE) {
  
  setwd("~/Part IV Project/R code")
  
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
  
  allSpeakers.df <- NULL
  VTlengths <- c()
  
  for (i in 1:numVTs)
  {
    allVowels.df <- compileMRIAreas(areaFiles = areaFiles, numVowels = numVowels,
                                    vowelNames = vowelNames, VTlist = VTlist, path = path,
                                    spk=VTlist[i], VTlengths <- VTlengths, interpN = interpN, smooth = smooth)
    VTlengths <- allVowels.df$lengths
    allSpeakers.df <- rbind(allSpeakers.df, allVowels.df$data)
  }
  
  output <- list("data" = allSpeakers.df, "numVTs" = numVTs, "VTlist" = VTlist, "vowelNames" = vowelNames, "lengths" = VTlengths)
  return(output)
  
}
