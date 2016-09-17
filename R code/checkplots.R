# Plots of VT02 and SM3 to see why their correlations are so poor with vowel-specific normalisation takes place.

graphics.off() 
par(mfrow=c(3,4), lwd=1)

# Each vowel
for (vow in NZE$vowelNames) {
  m <- grep(paste("\\b",vow,"\\b",sep=""), combined.df$vow)
  
  if (vow == Vowel.skip) {
    m <- m[m != intersect(m, grep(VT.skip, combined.df$spk))]
  }
  
  plot(1, type="n", axes=T, xlab="normalised distance", ylab="% area", xlim=c(0,1), ylim=c(0,1.05))
  
  # Each speaker
  for (i in 1:length(m)) {
    y <- unname(unlist(combined.df[m[i], 3:46]))
    VT <- combined.df[m[i],]$spk
    # VT02 is red, SM3 is blue, the rest are black
    if (VT == "VT02") {
      a <- "red"
      b <- 2
    } else if (VT == "SM3") {
      a <- "blue"
      b <- 2
    } else {
      a <- "black"
      b <- 1
    }
    lines(seq(0,1,1/43), y, col = a, lwd = b)
  }
  
  title(vow)
}
# 
# # Closer look at 'hod' - seems to be plotting itself twice for SM2 and VT02
# par(new=F, mfrow=c(1,1))
# plot(1, type="n", axes=T, xlab="normalised distance", ylab="% area", xlim=c(0,1), ylim=c(0,1.05))
# vow <- 'hod'
# m <- grep(paste("\\b",vow,"\\b",sep=""), combined.df$vow)
# for (i in 1:length(m)) {
#   y <- unname(unlist(combined.df[m[i], 3:46]))
#   VT <- combined.df[m[i],]$spk
#   # VT02 is red, SM3 is blue, the rest are black
#   t = "l"
#   if (VT == "VT02") {
#     a <- "red"
#     b <- 2
#   } else if (VT == "SM3") {
#     a <- "blue"
#     b <- 2
#   } else if (VT == "VT01") {
#     a <- "black"
#     b <- 1
#   } else {
#     t = "n"
#   }
#   lines(seq(0,1,1/43), y, col = a, lwd = b, type = t)
# }
# title("hod")
# 

# Check plots of PCs
par(new=F, mfrow=c(1,1))
plot(1, type="n", axes=T, xlab="index", ylab="PC1 x", xlim=c(0,11), ylim=c(-3,3))
for (VT in VTlist) {
  m <- grep(VT, combined.df$spk)
  pca <- prcomp(~., data = combined.df[m, -c(1:2,46)], scale=do.scale)
  t <- "l"
  if (VT == "VT02") {
    a <- "red"
    b <- 2
  } else if (VT == "SM3") {
    a <- "blue"
    b <- 2
  } else {
    a <- "black"
    b <- 1
  }
  lines(pca$x[,1], col = a, lwd = b, type = t)
}

par(new=F, mfrow=c(1,1))
plot(1, type="n", axes=T, xlab="index", ylab="PC1 rotation", xlim=c(0,44), ylim=c(-0.3,0.3))
for (VT in VTlist) {
  m <- grep(VT, combined.df$spk)
  pca <- prcomp(~., data = combined.df[m, -c(1:2,46)], scale=do.scale)
  t <- "l"
  if (VT == "VT02") {
    a <- "red"
    b <- 2
  } else if (VT == "SM3") {
    a <- "blue"
    b <- 2
  } else {
    a <- "black"
    b <- 1
  }
  lines(pca$rotation[,1], col = a, lwd = b, type = t)
}

