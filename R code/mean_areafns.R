# Mean area functions

# Plots the mean area functions for different demographics

# Written by Jenny Sahng
# 20/09/2016

rm(list=ls()) # clear workspace
graphics.off() # close all graphics windows

# Parameters ====================================================

path <<- "H:\\Documents\\Part IV Project\\All VT data"

# Normalise area functions? (F, "spk" for speaker-specific or "vow" for vowel-specific)
# Probably not a great idea for this one where we are literally comparing pure magnitudes
do.norm <- F

# Interpolate to how many points (if at all)?
M <- 44

# Divide NZE$data by 100 to convert to cm2? 
# Probs should do for this, see reason above.
do.cm2 <- T

# Fit a smoothing spline when reading in area functions
do.spline <- F

# The VT and vowel number to skip in correlations if it's missing (SM2 herd)
VT.skip <- "SM2"
Vowel.skip <- "herd"
row.skip <- 180

# Pre-amble ====================================================

source('~/Part IV Project/R code/Story (2005)/readAreaFunctions_Story.R')
AmE <- read.Story.data(interpN = M)
setwd("~/Part IV Project/R code")
source('~/Part IV Project/R code/readAreaFunctions_1Set.R')
NZE <- read.NZE.data(path = path, interpN = M, smooth = do.spline)

# Divide NZE by 100 to convert from mm2 to cm2
if (do.norm == F) { if (do.cm2) { NZE$data[,3:M+2] <- NZE$data[,3:M+2]/100 } }

# Combine datasets
numVTs <- AmE$numVTs + NZE$numVTs
VTlist <- c(NZE$VTlist, AmE$VTlist)

if (do.norm == "spk") {
  # Normalise data specific to speaker only
  maxArea <- vector(length = numVTs)
  for (i in 1:numVTs) {
    m <- grep(VTlist[i], combined.df$spk)
    maxArea[i] <- max(combined.df[m,3:46], na.rm = TRUE)
    combined.df[m,3:46] <- combined.df[m,3:46]/maxArea[i]
  }
} else if (do.norm == "vow") {
  # Normalise data specific to each individual vowel
  maxArea <- apply(combined.df[,3:46],1,max)
  combined.df[,3:46] <- combined.df[,3:46]/maxArea
} 
if (do.norm != F) {stopifnot(max(combined.df[,3:46], na.rm=T) == 1)}

# Combined mean ==================================================
# Age ==================================================
# Gender ==================================================
# Accent ==================================================
