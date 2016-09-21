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

# Speaker-specific normalisation?
do.norm <- F

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

# Speaker-specific normalisation
x <- 1
if (do.norm == "spk") {
  # Normalise data specific to speaker only
  maxArea <- vector(length = numVTs)
  for (i in 1:numVTs) {
    m <- grep(VTlist[i], combined.df$spk)
    maxArea[i] <- max(combined.df[m,3:46], na.rm = TRUE)
    combined.df[m,3:46] <- combined.df[m,3:46]/maxArea[i]
  }
  x <- 7
}


# Age ==================================================

y <- grep('^(VT03|VT04|VT05|VT06|VT07|VT10|VT12|SF|)', combined.df$spk[-row.skip]) # cutoff for young at 30 years
m <- grep('^(VT08|VT09|VT12|SM)', combined.df$spk[-row.skip])
s <- grep('^(VT01|VT02|VT11)', combined.df$spk[-row.skip])
y.mean <- apply(combined.df[y,1:M+2], 2, mean, na.rm = T)
m.mean <- apply(combined.df[m,1:M+2], 2, mean, na.rm = T)
s.mean <- apply(combined.df[s,1:M+2], 2, mean, na.rm = T)

par(lwd = 2)
plot(y.mean, type="l", col="purple3", main="Mean area functions for different age groups",
     ylab = expression(Average ~ cross-sectional ~ area ~ (cm^{2})),
     xlim = c(0,44), ylim = c(0,3.5/x))
lines(m.mean, type="l", lty=2, col="dodgerblue3")
lines(s.mean, type="l", lty=3, col="darkorange2")
legend("bottomleft", bty="n", c("20 - 30 years (10 speakers)", "30 to 55 years (6 speakers)",
       "55+ years (3 speakers)"), lty=c(1,2,3), col=c("purple3","dodgerblue3","darkorange2")) 


# Gender ==================================================

f <- grep('^(SF|VT02|VT06|VT07|VT08|VT11|VT12)', combined.df$spk[-row.skip])
f.no12 <- grep('^(SF|VT02|VT06|VT07|VT08|VT11)', combined.df$spk[-row.skip])
m <- grep('^(SM|VT01|VT03|VT04|VT05|VT9|VT10)', combined.df$spk[-row.skip])
f.mean <- apply(combined.df[f,1:M+2], 2, mean)
f.no12.mean <- apply(combined.df[f.no12,1:M+2], 2, mean)
m.mean <- apply(combined.df[m,1:M+2], 2, mean, na.rm = T)

plot(f.mean, type="l", col="deeppink", main="Mean area functions for female and male speakers",
     ylab = expression(Average ~ cross-sectional ~ area ~ (cm^{2})),
     xlim = c(0,44), ylim = c(0,3.5/x))
lines(f.no12.mean, type="l", lty=3, col="deeppink")
lines(m.mean, type="l", lty=2, col="dodgerblue")
legend("bottomleft", bty="n", c("Female", "Female with VT12 excluded", "Male"), lty=c(1,3,2), col=c("deeppink","deeppink","dodgerblue3"))

# Separate by accent

# NZE
nz.f <- grep('^(VT02|VT06|VT07|VT08|VT11|VT12)', combined.df$spk)
nz.m <- grep('^(VT01|VT03|VT04|VT05|VT9|VT10)', combined.df$spk)
nz.f.mean <- apply(combined.df[nz.f,1:M+2], 2, mean)
nz.m.mean <- apply(combined.df[nz.m,1:M+2], 2, mean, na.rm = T)

# AmE
am.f <- grep('^(SF)', combined.df$spk)
am.m <- grep('^(SM)', combined.df$spk)
am.f.mean <- apply(combined.df[am.f,1:M+2], 2, mean)
am.m.mean <- apply(combined.df[am.m,1:M+2], 2, mean, na.rm = T)

plot(nz.f.mean, type="l", col="black", main="Mean area functions for female and male speakers",
     ylab = expression(Average ~ cross-sectional ~ area ~ (cm^{2})),
     xlim = c(0,44), ylim = c(0,3.5/x))
lines(nz.m.mean, lty=2, col="black")
lines(am.f.mean, lty=1, col="red")
lines(am.m.mean, lty=2, col="red")
legend("bottomleft", bty="n", c("NZE Female", "NZE Male", "GenAm Female", "GenAm Male"),
       lty=c(1,2,1,2), col=c("black", "black", "red","red"))

# Trying to figure out why female area functions are bigger than men
f.max <- apply(combined.df[c(nz.f, am.f), 1:M+2], 1, max)
max(f.max)
mean(f.max)

m.max <- apply(combined.df[c(nz.m, am.m), 1:M+2], 1, max)
max(m.max[-70])
mean(m.max[-70])

# With distances from lips

nz.f.x <- apply(NZE$distances[nz.f,1:M+2], 2, mean)
nz.m.x <- apply(NZE$distances[nz.m,1:M+2], 2, mean)

plot(nz.f.x, nz.f.mean, type="l", col="black", main="Mean area functions for NZE female and male speakers",
     ylab = expression(Average ~ cross-sectional ~ area ~ (cm^{2})),
     xlab = "Distance from lips (mm)", xlim = c(0,180), ylim = c(0,3.5/x))
lines(nz.m.x, nz.m.mean, lty=2, col="black")
lines(am.f.mean, lty=1, col="red")
lines(am.m.mean, lty=2, col="red")
legend("bottomleft", bty="n", c("NZE Female", "NZE Male", "GenAm Female", "GenAm Male"),
       lty=c(1,2,1,2), col=c("black", "black", "red","red"))
legend("bottomleft", bty="n", c("NZE Female", "NZE Male"),
       lty=c(1,2,1,2), col=c("black", "black"))

# Accent ==================================================

NZE.mean <- apply(combined.df[1:132,1:M+2], 2, mean)
AmE.mean <- apply(combined.df[132:numVowels,1:M+2], 2, mean, na.rm = T)

plot(NZE.mean, type="l", col="black", main="Mean area functions for NZE and AmE",
     ylab = expression(Average ~ cross-sectional ~ area ~ (cm^{2})),
     xlim = c(0,44), ylim = c(0,3.5/x))
lines(AmE.mean, type="l", lty=2, col="red")
legend("bottomleft", bty="n", c("NZE", "AmE"), lty=c(1,2), col=c("black","red"))

# High vs. Low vowels

high <- grep('^(heed|hid|whod)', combined.df$vow)
low <- grep('^(had|hard|head|hod)', combined.df$vow)
mid <- grep('^(heard|hoard|hood|hud)', combined.df$vow)
high.mean <- apply(combined.df[high,1:M+2], 2, mean)
low.mean <- apply(combined.df[low,1:M+2], 2, mean, na.rm = T)
mid.mean <- apply(combined.df[mid,1:M+2], 2, mean, na.rm = T)

plot(high.mean, type="l", col="mediumorchid1", main="Mean area functions by vowel height",
     ylab = expression(Average ~ cross-sectional ~ area ~ (cm^{2})),
     xlim = c(0,44), ylim = c(0,4/x))
#lines(mid.mean, lty=2, col="mediumpurple2")
lines(low.mean, lty=2, col="darkturquoise")
#legend("bottomleft", bty="n", c("High vowels", "Mid vowels", "Low vowels"),
       #lty=c(1,2,3), col=c("mediumorchid1", "mediumpurple2", "darkturquoise"))
legend("bottomleft", bty="n", c("High vowels", "Low vowels"),
       lty=c(1,2), col=c("mediumorchid1", "darkturquoise"))


front <- grep('^(had|hard|head|heed)', combined.df$vow)
back <- grep('^(hoard|hod|hud|whod)', combined.df$vow)
central <- grep('^(heard|hid|hood|)', combined.df$vow)
front.mean <- apply(combined.df[front,1:M+2], 2, mean)
back.mean <- apply(combined.df[back,1:M+2], 2, mean, na.rm = T)
central.mean <- apply(combined.df[central,1:M+2], 2, mean, na.rm = T)

plot(front.mean, type="l", col="red", main="Mean area functions by vowel backness",
     ylab = expression(Average ~ cross-sectional ~ area ~ (cm^{2})),
     xlim = c(0,44), ylim = c(0,4/x))
lines(back.mean, lty=2, col="darkorange")
lines(central.mean, lty=3, col="darkgoldenrod1")
legend("bottomleft", bty="n", c("Front vowels", "Central vowels", "Back vowels"),
       lty=c(1,2,3), col=c("red", "darkorange", "darkgoldenrod1"))









