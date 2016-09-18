## Calculate LPC coefficients from area functions

# Written by Catherine Watson
# Adapted by Jenny Sahng
# Date: 18/09/2016

"calc.reflection.coef" <- function(areadata, diameter = FALSE) {
  # calculates reflection coefficents from the cross-sectional
  # areas, ordered from the lips to glottis.
  
  # Need to remember the last tube is the area at the glottis
  # (zero for a closed tube)
  
  # If vocal tract areas are diameters of sections (diameter=TRUE),
  # convert to cross-sectional areas
  
  if (diameter) { areadata <- ((areadata/2)**2)*pi }
  
  # The glottis is modelled as a tube of area 0. This provides a lossless tube model
  areadata <- c(areadata, 0)
  
  # Initialise vector for reflection coefficient
  refl.coef <- vector(mode="numeric",length=length(areadata))
  
  # first example(command)need to get reflection coefficent between lips and outside, so
  # set areadata[0]to be area outside lips and make this infinity. This
  # give the first reflection coefficent as 1 ( substitute into formulae
  # below to check)
  
  refl.coef[1] <-1
  
  # calculate reflection coefficent between lips and glottis
  for(i in 2:length(areadata))
        {
        refl.coef[i] <- (areadata[i-1]-areadata[i])/(areadata[i-1]+areadata[i])
        }
  
  return(refl.coef)
}
  
"rc2lpc.II"= function(reflcoef) {
  # this function converts reflection coefficients to linear prediction
  # coefficents using a recursive algorithm. This version uses the algorithm
  # in JMH's book p216
  # modified march 2nd 2007.
  
  # note this function expects that the first reflection coeffcient is 1
  if(reflcoef[1]!=1)
  {
  cat("reflcoef coef not in expected format, first coef must be 1")
  return()
  }
  
  # store values in matrix - recursive process which requires values
  # from previous iteration
  lpc.order <- length(reflcoef)
  lin.pred.coef <- matrix(nrow=lpc.order, ncol=lpc.order, byrow=TRUE)
  
  
  #recursive process
  lin.pred.coef[1,1] <- reflcoef[1]
  
  #the order p is one less than the length of rc vector
  for(j in 2:lpc.order)
  {
  lin.pred.coef[j,j] <- reflcoef[j]	
  	 for(i in 1:j-1)
  	 {
  	 lin.pred.coef[j,i] <- lin.pred.coef[j-1,i]-(reflcoef[j]*lin.pred.coef[j-1,j-i])
  	 }
  }
  
  #only return last row of matrix
  return(lin.pred.coef[lpc.order,])
}


"lpc2spec.III" =function(filtcoef,n=128)
{

# calculates spectrum from lpc coef. Could just zero extend and put
# into fft, but wanted to evalue my self
# modified 2/3/07 -
# filtercoef = number of LPC coef
# n = number of steps around ½ the unit circle


# make a vector of the negative powers of z
order <-1:length(filtcoef)

# make a row vector of the n steps around 1/2 the unit circle. Note: R
# vector default to be column vector, want this to be a row vector so
# must transpose.
tn <-seq(0,0.5,length=(n+1))
tn<-t(tn[1:n])
# calculate the power of the exponential(note how you make it imaginary)
# NOTE: MATRIX MULTIPLICATION
# "j times 2 pi times (0:0.5 in steps of n)" remember this has been normalized 
zpow <- 2i*pi*tn

# now solve the formulae H(z)= 1/A(z), where
# A(z) = 1 - SUM(i=1 to order)a[i]*z^-i
# R coerses the filtercoef vector so it can be multiplied by the z vector
hz <- (1-exp(-zpow))/(1-(filtcoef%*%exp(-order%*%zpow)))

# return the absolute value of this
return(Mod(hz))
}