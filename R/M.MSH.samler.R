#' @include LR.R multishift.R
#'
# Function: M.MSH.sampler
#
# Metropolis with multishift coupler
# LB: time of block for coelescing
# init: initial values of two paths (the most interest range)
# post: log of target or postorior distribution
# sigma: standard deviation for multshift proposal
# log: A logical value of T or F which allows to log form of posterior
#
# Authors: Majid Nabipoor, Duncan Murdoch
# revised: Oct. 2021

M.MSH.sampler<- function(LB,init, post,sigma, log=FALSE)
{
  nn<- length(init)
  d<- matrix(rep(0,nn*(LB+1)), nrow=LB+1)
  d[1,]<- init
  for(i in 1:LB)
  {
    y<-multishift(d[i,],sigma)
    u<- runif(1,0,1)
    ratio<- post(y)/post(d[i,])
    if(log==TRUE) {
                    u<-log(u)
                    ratio<-post(y)-post(d[i,])
                  }
    d[i+1,]<- ifelse(u<ratio,y,d[i,])
  }
  return(d)
}

