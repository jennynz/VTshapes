# Reading area functions from Story (2005)

# Reads area functions scraped from Story (2005) and delimited into csv files. 

# Written by Jenny Sahng
# 14/09/2016

"read.Story.data" <- function(interpN = FALSE) {
  
  setwd("~/Part IV Project/R code/Story (2005)")

  # List of VT name strings (SF1, SM1...)
  areaFiles <- dir(pattern = "^S[FM]")
  numVTs <- length(areaFiles)
  VTlist <- vector(length = numVTs)
  for(i in 1:numVTs) {
    VTlist[i] <- unlist(strsplit(areaFiles[i],"\\."))[1] # Remove .csv
  }
  
  # List of vowel names (hadd, heed...)
  vowelNames <- c("heed","hid","head","herd","had","hud","hard","hod","hoard","hood","whod")
  numVowels <- length(vowelNames)
  
  "read.VT" <- function(VTfile)
  {
  	# Can decide how many values to interpolate over, or leave it with the default which is 29 (the number of data points in the area functions/numrows in .txt files)
    nmax <- dim(read.csv(VTfile, header=F))[1]
    if (interpN) {
      n <- interpN
    } else {
      n <- nmax
    }

  	# Make data matrix and vowel names vector
  	area.fn <- matrix(nrow = numVowels, ncol = nmax, byrow = T)
  
  	# Read area function csv for all vowels, interpolate area functions, write to data matrix
  	area.fn <- read.csv(VTfile, header=F)
  
  	# Reverse order so that X1 = lips and X44 = glottis, then rotate.
  	area.fn <- t(apply(area.fn, 2, rev))
  	
  	# No need to linearly interpolate, data already resampled with a cubic spline,
  	# 44 area sections have equal length increments.
  		
  	# Unless doing resonance and you need the same number of area data points for NZE and AmE.
  	VTareas <- matrix(nrow = numVowels, ncol = n, byrow = T)
  	if (interpN) {
  	  for (i in 1:numVowels) {
  	    # Check for NA (assumes whole row is NA since it only checks the first value)
  	    if (is.na(area.fn[i,1])) {
  	      VTareas[i,] <- area.fn[i,1:n]
  	    } else {
  	      VTareas[i,] <- approx(area.fn[i,], n=n)$y 
  	    }
  	  }
  	} else {
  	  VTareas <- area.fn
  	}
  	
  	# Create data frame with speaker labels, vowel labels, and cross-sectional areas. 
  	VT <- unlist(strsplit(VTfile, "\\."))[1]
  	return(data.frame(spk = factor(rep(VT,numVowels)), vow = factor(vowelNames), VTareas))
  }
  
  allAmE.df <- NULL
  
  for (VT in areaFiles)
  {
  	allVowels.df <- read.VT(VTfile=VT)
    allAmE.df <- rbind(allAmE.df, allVowels.df)
  }
  
  row.names(allAmE.df) <- NULL
  
  output <- list("data" = allAmE.df, "numVTs" = numVTs, "VTlist" = VTlist, "vowelNames" = vowelNames)
  return(output)
  
}

"read.Story.VTlengths" <- function() {
  setwd("~/Part IV Project/R code/Story (2005)")
  vtl <- read.csv("vocal_tract_lengths.csv", header=F)
  vtl <- as.vector(unname(unlist(t(vtl[,-1]))))
  return(vtl)
}
