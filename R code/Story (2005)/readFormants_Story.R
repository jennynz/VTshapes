# Reading formants from Story (2005)

# Reads formants (natural formants FXn and 'calculated formants' FXc which are
# actually resonances calculated from area funcions) scraped from Story (2005)
# and delimited into csv files.

# Written by Jenny Sahng
# 20/09/2016

"read.Story.formants" <- function() {
  
  setwd("~/Part IV Project/R code/Story (2005)")
  
  # List of VT name strings (SF1, SM1...)
  areaFiles <- dir(pattern = "(formants)")
  numVTs <- length(areaFiles)
  numVowels <- dim(read.csv(areaFiles[1]))[1]
  VTlist <- vector(length = numVTs)
  for(i in 1:numVTs) {
    VTlist[i] <- unlist(strsplit(areaFiles[i],"\\_"))[1] # Remove .csv
  }
  
  # List of vowel names (hadd, heed...)
  vowelNames <- c("heed","hid","head","herd","had","hud","hard","hod","hoard","hood","whod")
  numVowels <- length(vowelNames)
  
  AmEformants <- NULL
  
  for (i in 1:numVTs) {
    formants.df <- data.frame(spk = factor(rep(VTlist[i], numVowels)), vow = factor(vowelNames), read.csv(areaFiles[i], header=T))
    AmEformants <- rbind(AmEformants, formants.df)
  }
  
  output <- list("data" = AmEformants, "numVTs" = numVTs, "VTlist" = VTlist, "vowelNames" = vowelNames)
  return(output)
  
}

