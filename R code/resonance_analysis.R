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

# Number of resonances to pick out
numRes <- 3

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
row.skip <- 180

# Pre-amble ====================================================

source('~/Part IV Project/R code/Story (2005)/readAreaFunctions_Story.R')
AmE <- read.Story.data(interpN = M)
setwd("~/Part IV Project/R code")
source('~/Part IV Project/R code/readAreaFunctions_1Set.R')
NZE <- read.NZE.data(path = path, interpN = M+1, smooth = do.spline)
# Interpolate one more because the value for NZE is zero, which needs to be
# ommitted before passing into calc.reflection.coef
NZE$data <- NZE$data[,-length(NZE$data)]

# Combine datasets
numVTs <- AmE$numVTs + NZE$numVTs
VTlist <- c(NZE$VTlist, AmE$VTlist)

# Switch order of NZE data to match order of vowels in AmE
NZE.switched <- NZE$data
for (i in 1:NZE$numVTs) {
  m <- grep(VTlist[i], NZE$data$spk)
  for (j in 1:length(NZE$vowelNames)) {
    n <- intersect(m, grep(paste("\\b",AmE$vowelNames[j],"\\b",sep=""), NZE$data$vow))
    k <- m[1] + j - 1
    NZE.switched[k,] <- NZE$data[n,]
  }
}
vowelNames <- AmE$vowelNames

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
vtspec <- t(apply(LPC, 2, lpc2spec.III, n = 512))

## Sampling frequency of each vtspec (fs in Hz)
# M = number of cross-sectional areas (defined earlier)
L <-  c(NZE$lengths/10, read.Story.VTlengths()) # L = length in cm
c = 3400        # c = speed of sound in air (cm/s)
fs <- (M * c) / (2 * L)

# Scaled frequency bins for each spectrum. Should be 198 x 512
freqbins <- matrix(rep(1:512, numVowels), ncol = 512, byrow = T)
freqbins <- freqbins * fs / (2*512) 

# Check that last column is half the sampling frequency.
# sum(freqbins[,512]==fs/2)#

## Plot a few to visually check
par(mfrow=c(3,2))
plot(freqbins[1,],log10(vtspec[1,]),type="l")
plot(freqbins[2,],log10(vtspec[2,]),type="l")
plot(bark(freqbins[1,]),log10(vtspec[1,]),type="l")
plot(bark(freqbins[2,]),log10(vtspec[2,]),type="l")

## find out the min and max fs/2 value - that will be around the maximum
## frequency value for all spectrums (want all spectrums to be on same scale)
min(freqbins[,512], na.rm = T)
max(freqbins[,512], na.rm = T)

############## NEED TO FINISH OFF ####################

# Not sure what is happening here with truncation etc. Don't know what endbin is.

## find the index for each frequency bin vector which is the maximum frequency value that is less than or equal to the min fs/2 value

##Want to see what actuall frequency value this equates too

# diag(freqbins.t[1:110,endbin[1:110]])
# 
# ## what is the difference between these values and the minimum fs/2 value
# diag(freqbins.t[1:110,endbin[1:110]])-min(freqbins.t[,512])
# 
# ## and what is this difference in Bark?
# bark(diag(freqbins.t[1:110,endbin[1:110]]))-bark(min(freqbins.t[,512]))
# 
# trunc.freqbinb=matrix(NA,ncol=256,nrow=110,byrow=TRUE)
# trunc.vtspecb=matrix(NA,ncol=256,nrow=110,byrow=TRUE)
# 
# for(i in 1:110)
# {
#   foo=approx(bark(freqbins.t[i,1:endbin[i]]),vtspec[i,1:endbin[i]],n=256)
#   trunc.freqbinb[i,1:256]=foo$x[1:256]
#   trunc.vtspecb[i,1:256]=foo$y[1:256]
# }

# PCA on spectra for correlations ===========================================================

# # Bark scale the spectra
# # Omitting the first column because they are zeros and NaNs
# vtspec <- bark(vtspec[,-1])
# 
# # To take logs of spectra, replace zero ammplitudes in spectrum with very small number
 which <- vtspec==0
 vtspec[which] <- 10^-16
# 
# barkspec <- prcomp(log10(vtspec[,1:200]), scale=T)
# summary(barkspec)
# 
# ## PCA plots from areas and resonances
# par(mfrow = c(1,2))
# eplot(barkspec$x[,1:2], labs = combined.df$vow, centroid = T, main = "PC of Bark scaled log spectra")
# eplot(foo3$x[,1:2], labs = vowelNames, centroid = TRUE, formant = T)
# eplot(barkspec$x[,1:2], labs = combined.df$vow, centroid = T, main = "PC of Bark scaled log spectra", formant = T)
# eplot(foo3$x[,1:2], labs = vowelNames, centroid = TRUE, formant = T, main = "PC of areas")
# 
# ## Plotting the rotation vectors.
# plot(trunc.freqbinb[1,1:200], barkspec$rotation[,1], main = "resonances, pc1", ylim = c(-0.15,0.15))
# plot(trunc.freqbinb[1,1:200], barkspec$rotation[,2], main = "resonances, pc2", ylim = c(-0.15,0.15))
# plot(foo3$rotation[,1], main = "areas, pc1", ylim = c(-0.4,0.3))
# plot(foo3$rotation[,2], main = "areas, pc2", ylim = c(-0.4,0.3))
# 
# 

# Calculating resonances ======================================================

# Identify peaks 
tol <- 5
vtres <- matrix(ncol = numRes, nrow = numVowels, byrow = T)
resfreq <- matrix(ncol = numRes, nrow = numVowels, byrow = T)
for(i in 1:numVowels){
  
  # Double differentiate
  dxdt2 <- diff(diff(vtspec[i,]))
  
  # values less than or equal to zero (where the peaks are, maxima)
  numbins <- 1:512
  index <- numbins[dxdt2 <= 0]
  index <- index[-c(1,length(index))]
  
  if (is.na(index[1]) == F) {
      
    #Ignore indices if they're too close - a 'peak' might just be two very similar
    #values, so whilst by eye can clearly pick out peaks, can't get these
    #automatically by doing just the second difference. If there are two 'peaks'
    #which are very close together (within a certain tolerance), take the highest
    #one.
    checked <- c()
    j <- 1
    is.similar <- T
    while (j < length(index)) {
      while (is.similar == T) {
        if ( (abs(index[j] - index[j+1])) < tol ) {
          j <- j + 1  
          if (j >= length(index)) { break }
        } else {
          is.similar <- F
        }
      }
      
      checked <- c(checked, index[j])
      j <- j + 1
      is.similar <- T
    }
    
    # Grab the spectra values at those peak points, and the frequencies that correspond in the frequency bins.
    vtres[i,] <- vtspec[i, checked[1:numRes]+1]
    resfreq[i,] <- freqbins[i, checked[1:numRes]+1]    
  }

}

# PCA on resonances for R1-R2 plots ===========================================================

pca.res <- prcomp(na.omit(resfreq))
pca.res.bark <- prcomp(bark(na.omit(resfreq))) 

r1 <- resfreq[-row.skip,1]
r2 <- resfreq[-row.skip,2]
r1r2 <- cbind(r1,r2)

colpalette <- c("firebrick4","chocolate4","darkgoldenrod","chartreuse4","aquamarine4",
                "darkcyan","deepskyblue4","darkslateblue","darkorchid4","deeppink4","indianred4")

combined.df$vow <- factor(combined.df$vow, levels = vowelNames)
levels(combined.df$vow) <- c() # copy and paste IPA symbols from IPAvowels_story.txt

par(mfrow = c(1,1))
# # PC1-PC2 on area functions
# eplot(foo3$x[,1:2],labs=allMRI.df[,3],centroid=TRUE,formant=T,main="Pareas")
# # PCA on bark scaled spectra
# eplot(x=pc.vtspecb$x[,1:2],labs=allMRI.df16$vow,centroid=T,formant=T,main="bark scaled spectra,all")
# # PCA on resonances
# eplot(pca.res$x[,1:2], labs = as.character(combined.df[-180,2]), centroid = T,
#       main="PCs of resonant frequencies for combined 18 VT data set",
#       formant = T, doellipse = F)
# # PCA on bark scaled resonances
# eplot(pca.res.bark$x[,1:2], labs = as.character(combined.df[-180,2]), centroid = T,
#       main="PCs of Bark-scaled resonant frequencies for combined 18 VT data set",
#       formant = T, doellipse = F)
# title("Centroids")

## Resonance plots (R1/R2)

# Centroids
eplot(r1r2, labs = as.character(combined.df[-180,2]), centroid = T,
      main="Centroids of vowels plotted by resonant frequencies in combined 18 VT data set",
      xlab = "R2", ylab = "R1", formant = T, doellipse = F, col = colpalette,
      xlim = c(80,270), ylim = c(20,80))
# Bark-scaled
eplot(bark(r1r2), labs = as.character(combined.df[-180,2]), centroid = T,
      main="PCs of Bark-scaled resonant frequencies for combined 18 VT data set",
      xlab = "R2", ylab = "R1", formant = T, doellipse = F)

# Clusters
eplot(r1r2, labs = as.character(combined.df[-180,2]), dopoints = T,
      main="All vowels plotted by resonant frequencies in combined 18 VT data set",
      xlab = "R2", ylab = "R1", formant = T, doellipse = T, col = colpalette)

# Centroids for NZE and AmE compared
par(mfrow=c(1,2))
eplot(r1r2[1:132,], labs = as.character(combined.df[1:132,2]), centroid = T, main="NZE centroids",
      xlab = "R2", ylab = "R1", formant = T, doellipse = F, col = colpalette,
      xlim = c(80,270), ylim = c(20,80))
eplot(r1r2[133:197,], labs = as.character(combined.df[c(133:179,181:198),2]), centroid = T, main="AmE centroids",
      xlab = "R2", ylab = "R1", formant = T, doellipse = F, col = colpalette,
      xlim = c(80,270), ylim = c(20,80))

# Plot together
par(new=F, mfrow=c(1,1))
eplot(r1r2[1:132,], labs = as.character(combined.df[1:132,2]), centroid = T, main="NZE and AME centroids",
      xlab = "R2", ylab = "R1", formant = T, doellipse = F, col = "black",
      xlim = c(80,270), ylim = c(20,90))
par(new=T)
eplot(r1r2[133:197,], labs = as.character(combined.df[c(133:179,181:198),2]), centroid = T,
      xlab = "R2", ylab = "R1", formant = T, doellipse = F, col = "red",
      xlim = c(80,270), ylim = c(20,90))
legend("topleft", bty="n", c("NZE", "AmE"), lty=c(1,1), col=c("black","red"))



