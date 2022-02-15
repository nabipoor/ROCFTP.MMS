#' @include LR.R
#'
# Function: multis                                                                                                                                                                                                                                                                                                                                                                         hift
#
# multishift coupler: takes initial value s with sigma and propose a new value
#
# Authors: Majid Nabipoor, Duncan Murdoch
# revised: Oct. 2021

multishift<- function(s,sigma)
{
  m<-L.R(sigma)
  X<- runif(1,m[1],m[2])
  f<-floor((s+m[2]-X)/(m[2]-m[1]))*(m[2]-m[1])+X
  return(f)
}
