# Reading area functions from Story (2005)

# Reads area functions scraped from Story (2005) and delimited into csv files. 

# Written by Jenny Sahng
# 14/09/2016

rm(list=ls()) # clear workspace
graphics.off() # close all graphics windows
setwd("~/Part IV Project/R code/Story (2005)")

# List of VT name strings (SF1, SM1...)
areaFiles <- dir(pattern = "^S[FM]")
numVTs <- length(areaFiles)
VTlist <- vector(length = numVTs)
for(i in 1:numVTs) {
  VTlist[i] <- unlist(strsplit(areaFiles[i],"\\."))[1] # Remove .csv
}

# List of vowel names (hadd, heed...)
numVowels <- 11
vowelNames <- c("heed","hid","head","herd","had","hud","hard","hod","hoard","hood","whod")

read.Story.data<-function(VTfile)
{
	# Can decide how many values to interpolate over, or leave it with the default which is 29 (the number of data points in the area functions/numrows in .txt files)

	# Make data matrix and vowel names vector
	area.fn <- matrix(nrow=numVowels,ncol=44,byrow=T)

	# Read area function csv for all vowels, interpolate area functions, write to data matrix
	area.fn <- read.csv(VTfile, header=F)

	# No need to linearly interpolate, data already resampled with a cubic spline,
	# 44 area sections have equal length increments.

	# Reverse order so that X1 = lips and X44 = glottis, then rotate.
	area.fn <- t(apply(area.fn, 2, rev))
	
	# Create data frame with speaker labels, vowel labels, and cross-sectional areas. 
	VT <- unlist(strsplit(VTfile, "\\."))[1]
	return(data.frame(spk = factor(rep(VT,numVowels)), vow = factor(vowelNames), area.fn))
}

allSpeakers.df <- NULL

for (VT in areaFiles)
{
	allVowels.df <- read.Story.data(VTfile=VT)
	allSpeakers.df <- rbind(allSpeakers.df, allVowels.df)
}

row.names(allSpeakers.df) <- NULL

