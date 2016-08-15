# Reading in area function data derived from MRI images.

# Area functions (cross-sectional area vs. distance from lips)are read into R and interpolated with the same number of points as the original matrix.
# Specific to the current file structure in project directory.
# Assumes one set only. See the original file plottingDanielsMRIdata.txt to use both sets.
# N.B. VT01 is missing data for hood. A dummy txt file named hood.txt has been created, with 29 x 2 table of N/As

# Adapted by Jenny Sahng
# 05/08/2016



rm(list=ls()) # clear workspace
graphics.off() # close all graphics windows

path<<-"H:\\Documents\\Part IV Project\\All VT data"

# List of VT name strings (VT01, VT02...)
VTlist <- list.dirs(path, recursive=FALSE, full.names=FALSE)
numVTs <- length(VTlist)

# List of vowel names (hadd, heed...)
areaFiles <- dir(paste(path,VTlist[1],"distance_area",sep="\\"))
numVowels <- length(areaFiles)
vowelNames <- vector(length=numVowels)
for(i in 1:numVowels) {
	vowelNames[i]<- unlist(strsplit(areaFiles[i],"\\."))[1] # Remove .txt
}

compileMRIAreas<-function(spk="VT01",interpN=FALSE)
{
	# Can decide how many values to interpolate over, or leave it with the default which is 29 (the number of data points in the area functions/numrows in .txt files)

	# Make data matrix and vowel names vector
	alldat <- matrix(nrow=numVowels,ncol=29,byrow=T)

	# for each speaker, go into their directory, reading the vocal tract data for each vowel, interpolate the cross-sectional areas
	#use file name to determine vowel name, write the cross-sectional areas - only to data matrix
	for(i in 1:numVowels)
	{
		filepath <- paste(path,spk,"distance_area",areaFiles[i],sep="\\")
		datfile <- read.table(filepath)

		if (interpN) { n <- interpN }
		else { n <- nrow(datfile) }

		# Need to linearly interpolate data, as different distance step in oral region than pharyngeal region.
		if ( (spk != "VT01") && (areaFiles[i] != "hood.txt") ) {
			LinDatfil <- approx(datfile[,1],datfile[,2],n=n)
			alldat[i,] <- LinDatfil$y
		} else {
			# Coerce the list into a double type so it's the same of the output from LinDatfil$y
			y <- as.numeric(unlist(datfile))
			# Extend the vector of NAs for VT01 "hood.txt" to fit in combined data frame
			if (length(y) < n) {
				# Fortunately, R fills in empty slots on a list with NA's anyway!
				y[length(y)+(n-length(y))] = NA
			} else if (length(y) > n) {
				y <- y[1:n]
			}

			# Add to alldat
			alldat[i,] <- y
		}
	}

	# Create data frame with speaker labels, vowel labels, and cross-sectional areas. 
	alldat.df=data.frame(spk=factor(rep(spk,numVowels)),vow=factor(vowelNames),alldat)
	return(alldat.df)
}

allSpeakers.df <- NULL

for (i in 1:numVTs)
{
	allVowels.df <- compileMRIAreas(spk=VTlist[i])
	allSpeakers.df <- rbind(allSpeakers.df, allVowels.df)
}

# Omit VT01 hood.txt since they're all NAs.
allSpeakers.df = allSpeakers.df[-9,]
