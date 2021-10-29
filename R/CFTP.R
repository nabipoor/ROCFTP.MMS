
#' @include LR.R multishift.R M.MSH.sampler.R
#'
# Function: CFTP
#
# Coupling From The Past generate one block with coalescence
# LB: time of block for coalescing
# start: initial values of two paths (the most interest range)
# post: log of target or posterior distribution
# sigma: standard deviation for multshift proposal
# log: A logical value of T or F which allows to log form of posterior
#
# Authors: Majid Nabipoor, Duncan Murdoch
# revised: Oct. 2021

CFTP<- function (LB,start, post,sigma, log=FALSE)
{
  allresult<- NULL
  y<- NULL
  init<- start
  T<- 0
  r<-0
  repeat{
    samples<-  M.MSH.sampler(LB,init, post, sigma, log)
    T<- T+ nrow(samples)
    r<-r+1
    na<-matrix(c(rep(NA, (T-1)*length(start)),start), nrow=T, byrow=TRUE)
    allresult<- cbind(rbind(allresult, samples),na)
    y<- samples
    init<- allresult[T,]
    if(length(unique(samples[nrow(samples),]))==1 || r>50) break
  }
  ifelse(r<50,return(y),return(noquote("Error: Change length of coalescence block or sigma or log parameter")))
}

