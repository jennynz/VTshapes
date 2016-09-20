# Mean area functions

# Plots the mean area functions for different demographics

# Written by Jenny Sahng
# 20/09/2016

rm(list=ls()) # clear workspace
graphics.off() # close all graphics windows

# Parameters ====================================================

path <<- "H:\\Documents\\Part IV Project\\All VT data"

# Interpolate to how many points (if at all)?
M <- 44

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
NZE$data[,1:M+2] <- NZE$data[,1:M+2]/100

# Combine datasets
numVTs <- AmE$numVTs + NZE$numVTs
VTlist <- c(NZE$VTlist, AmE$VTlist)
combined.df <- rbind(NZE$data, AmE$data)
numVowels <- dim(combined.df)[1]


# Age ==================================================

y <- grep('^(VT03|VT04|VT05|VT06|VT07|VT10|VT12|SF|)', combined.df$spk[-row.skip]) # cutoff for young at 30 years
m <- grep('^(VT08|VT09|VT12|SM)', combined.df$spk[-row.skip])
s <- grep('^(VT01|VT02|VT11)', combined.df$spk[-row.skip])
y.mean <- apply(combined.df[y,1:M+2], 2, mean, na.rm = T)
m.mean <- apply(combined.df[m,1:M+2], 2, mean, na.rm = T)
s.mean <- apply(combined.df[s,1:M+2], 2, mean, na.rm = T)

par(lwd = 2)
plot(y.mean, type="l", col="darkorange2", main="Mean area functions for different age groups",
     ylab = expression(Average ~ cross-sectional ~ area ~ (cm^{2})),
     xlim = c(0,44), ylim = c(0,3.5))
lines(m.mean, type="l", lty=2, col="dodgerblue3")
lines(s.mean, type="l", lty=3, col="purple3")
legend("bottomleft", bty="n", c("20 - 30 years (10 speakers)", "30 to 55 years (6 speakers)",
       "55+ years (3 speakers)"), lty=c(1,2,3), col=c("darkorange2","dodgerblue3","purple3")) 


# Gender ==================================================

f <- grep('^(SF|VT02|VT06|VT07|VT08|VT11|VT12)', combined.df$spk[-row.skip])
m <- grep('^(SM|VT01|VT03|VT04|VT05|VT9|VT10)', combined.df$spk[-row.skip])
f.mean <- apply(combined.df[f,1:M+2], 2, mean)
m.mean <- apply(combined.df[m,1:M+2], 2, mean, na.rm = T)

plot(f.mean, type="l", col="deeppink", main="Mean area functions for female and male speakers",
     ylab = expression(Average ~ cross-sectional ~ area ~ (cm^{2})),
     xlim = c(0,44), ylim = c(0,3.5))
lines(m.mean, type="l", col="dodgerblue")
legend("topleft", bty="n", c("Female", "Male"), lty=c(1,1), col=c("deeppink","dodgerblue3"))


# Accent ==================================================

NZE.mean <- apply(combined.df[1:132,1:M+2], 2, mean)
AmE.mean <- apply(combined.df[132:numVowels,1:M+2], 2, mean, na.rm = T)

plot(NZE.mean, type="l", col="black", main="Mean area functions for NZE and AmE",
     ylab = expression(Average ~ cross-sectional ~ area ~ (cm^{2})),
     xlim = c(0,44), ylim = c(0,3.5))
lines(AmE.mean, type="l", col="red")
legend("topleft", bty="n", c("NZE", "AmE"), lty=c(1,1), col=c("black","red"))













