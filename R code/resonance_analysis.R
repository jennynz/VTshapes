# Resonance analysis for all NZE and AmE datasets combined

# Calculates resonances R1 and R2 from area functions.
# Plots vowels on R1/R2 plots, compares these with P2/P1 plots.
# Performs correlations between principal components of resonances.

# Written by Jenny Sahng
# 18/09/2016

rm(list=ls()) # clear workspace
graphics.off() # close all graphics windows

# Parameters ====================================================

path <<- "H:\\Documents\\Part IV Project\\All VT data"

# Number of cylinders
M <- 16

# Final csv file with all information (correlation and variances)
filename <- "resonances_correlations.csv"

# Normalise area functions? (F, "spk" for speaker-specific or "vow" for vowel-specific)
do.norm <- F

# Scale to unit variance before PCA?
do.scale <- F

# Divide NZE$data by 100 to convert to cm2? (consistent with Story, but does not affect correlations at all)
# Would only do for plotting purposes (to get things fitting on the same y-axis) and if not normalising.
do.cm2 <- F

# Fit a smoothing spline when reading in area functions
do.spline <- F

# The VT and vowel number to skip in correlations if it's missing (SM2 herd)
VT.skip <- "SM2"
Vowel.skip <- "herd"

# Principal components to analyse
p.max <- 3

# Pre-amble ====================================================

source('~/Part IV Project/R code/Story (2005)/readAreaFunctions_Story.R')
AmE <- read.Story.data(interpN = M)
source('~/Part IV Project/R code/readAreaFunctions_1Set.R')
NZE <- read.NZE.data(path = path, interpN = M+1, smooth = do.spline)
# Interpolate one more because the value for NZE is zero, which is going to get
# cut when passing into calc.reflection.coef


# Combine datasets
numVTs <- AmE$numVTs + NZE$numVTs
VTlist <- c(NZE$VTlist, AmE$VTlist)

# Switch order of NZE data to match order of vowels in AmE
NZE.switched <- NZE$data[,-length(NZE$data)]
for (i in 1:NZE$numVTs) {
  m <- grep(VTlist[i], NZE$data$spk)
  for (j in 1:length(NZE$vowelNames)) {
    n <- intersect(m, grep(paste("\\b",AmE$vowelNames[j],"\\b",sep=""), NZE$data$vow))
    k <- m[1] + j - 1
    NZE.switched[k,] <- NZE$data[n,]
  }
}

# Divide NZE by 100 to convert from mm2 to cm2
if (do.norm == F) {
  if (do.cm2) { NZE.switched[,3:46] <- NZE.switched[,3:46]/100 }
}

# Get rid of last value in NZE (zeros)
combined.df <- rbind(NZE.switched[,-(n+3)], AmE$data)
numVowels <- dim(combined.df)[1]

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

# Resonances with LPC ====================================================

source('~/Part IV Project/R code/LPCfromArea.R')

# Reflection coefficients from area functions NZE already have 0 at the end of
# their area functions. To keep lengths the same, might have to just omit the
# last value in the AmE datasets.
# RC.df <- combined.df
# for (i in 1:numVowels) {
#   RC.df[i, -c(1,2)] <- calc.reflection.coef(unname(unlist(combined.df[i, -c(1,2,46)])))
# }
RC <- apply(combined.df[,-c(1:2)], 1, calc.reflection.coef)
LPC <- apply(RC, 2, rc2lpc.II)
spectrum <- apply(LPC, 2, lpc2spec.III, n = 512)

## Quick check of spectra

# Same vowel
plot(spectrum[,11],type="l")
lines(spectrum[,22],col="red")
lines(spectrum[,33],col="blue")
lines(spectrum[,44],col="green")

# Different vowels for same VT
plot(spectrum[,2],type="l")
lines(spectrum[,3],col="red")
lines(spectrum[,6],col="blue")
lines(spectrum[,9],col="green")

## Sampling frequency of each spectrum (fs in Hz)
# M = number of cross-sectional areas (defined earlier)
L <-  c(NZE$lengths, read.Story.VTlengths) # L = length in cm
c = 3400        # c = speed of sound in air (cm/s)
fs <- (M * c) / (2 * L)

# PCA on spectra ===========================================================











