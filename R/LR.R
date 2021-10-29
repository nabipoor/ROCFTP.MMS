# Function: LR
#
# generate Left and Right from normal distribution mean=0 and sd=sigma
# generate randmn z and u separately and store them before using in calculation
# in order to keep independence between u and z
#
# Authors: Majid Nabipoor, Duncan Murdoch
# revised: October 2021

L.R<- function(sigma)
{
  z<- rnorm(1,0,sigma)
  u<- runif(1,0,dnorm(z,sd=sigma))
  main<- sigma*sqrt(-2*log(sqrt(2*pi)*u*sigma, base = exp(1)))
  m<- c(-main,main)
  return(m)
}
