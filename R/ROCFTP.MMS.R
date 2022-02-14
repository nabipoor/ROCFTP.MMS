#' @include LR.R multishift.R M.MSH.sampler.R CFTP.R
NULL

#'  Perfect Sampling
#'
#'  \code{ROCFTP.MMS}, Read Once Coupling From The Past, with Metropolis-Multishift
#'  is used to generate a perfect sample for given posterior density based on the
#'  two extreme starting paths, minimum and maximum of the most interest range of
#'  the posterior. It uses the monotone random operation of multishift coupler
#'  which allows to sandwich all of the state space in one point. It means both
#'  Markov Chains starting form the maximum and minimum will be coalesced.
#'  The generated sample is independent from the starting points. It is useful
#'  for mixture posteriors too. This function's output is a real value as an exact
#'  draw from the posterior distribution.
#'
#' @param LB defines the length of each block. The algorithm starts new blocks to
#' find a coalescence of the two Markov chains started from extreme points of the
#' most interest range. So, if it is too small the algorithm after 50 blocks
#' repetition will give error message, and if it is too large then it will
#' increase generation time.
#'
#'@param start is a vector of the initial values of the two extreme points of the most interest
#' range of posterior.
#'
#' @param post is the posterior which is defined in the form of an R function.
#'
#' @param sigma is a real value for standard deviation of multishift coupler. Multishift coupler
#' is constructed based on normal density. If the posterior is a mixture
#' distribution or a multi-modal distribution, then sigma should be chosen in such
#' a way that Markov chains easily moves between modes, and if the sigma is
#' chosen small; the Markov chains may trap in one mode and don't coalesce.
#'
#'@param log has the default value of FALSE for density function of posteriors.
#' If TRUE; then posterior should be defined in log form.
#'
#' @section References:
#' Nabipoor M, Murdoch D. (2010) ROCFTP With Metropolis-Multishift Coupler, Summer
#' Research, Department of Statistical and actuarial sciences, University of
#' Western Ontario
#' @examples
#' #Unimodal posterior
#' post<- function(x)
#' {
#'   dnorm(x, mean=30, sd=1)
#' }
#' start<- c(20,40)
#' LB=30
#' ROCFTP.MMS(LB,start, post,1)
#' #Generate n i.i.d. exact sample
#' RG <- function(i){ROCFTP.MMS(30, c(20,40), post, 1)}
#' n <- 5
#' mrtx <- matrix(1:n, ncol=1)
#' dat <- apply(mrtx,1,RG)
#' qqnorm(dat, pch = 16, frame = FALSE)
#' qqline(dat, col = "steelblue", lwd = 3)
#'
#' # multimodal posterior
#' post <- function(x)
#' {
#'  0.2*dnorm(x, mean=-5, sd=1)+0.2*dnorm(x, mean=5, sd=1)+0.6*dnorm(x, mean=15, sd=1)
#' }
#' start <- c(-15,25)
#' LB <- 116
#' sigma <- 3.5
#' ROCFTP.MMS(LB,start, post,sigma) #generates one exact sample
#' #Generate n i.i.d. exact sample
#' RG <- function(i){ROCFTP.MMS(116, c(-15,25), post, 3.5)}
#' n <- 5
#' mrtx <- matrix(1:n, ncol=1)
#' apply(mrtx,1,RG)
#'
#' #log form of posterior
#' post<- function(x) {dnorm(x, mean=30, sd=1, log=TRUE)}
#' start<- c(20,40)
#' LB=100
#' sigma=0.5
#' ROCFTP.MMS(LB,start, post, sigma, log=TRUE)
#' @author
#' Majid Nabipoor: nabipoor@@ualberta.ca
#' Duncan Murdoch: murdoch.duncan@@gmail.com
#' @importFrom stats dnorm rnorm runif
#' @import vctrs
#' @export

ROCFTP.MMS<- function(LB, start, post, sigma, log=FALSE)
{
  if (LB <= 0 | length(LB) > 1 | is.numeric(LB)==FALSE) {
    return(noquote("Error: LB should be an integer"))
  }
  if (length(start) != 2 | is.vector(start)==FALSE) {
    return(noquote("Error: The length of start vector should be two"))
  }
  if (sigma <= 0 | length(sigma) > 1 | is.numeric(sigma)==FALSE) {
    return(noquote("Error: sigma should be a positive number"))
  }
  allresults<- CFTP(LB,start, post,sigma, log)
  if(is.character(allresults)) {return(allresults)} else {
  allresults<- allresults[,(ncol(allresults)-length(start)+1):(ncol(allresults))]
  allresults<- cbind(rep(NA,nrow(allresults)), allresults)
  x<- allresults[nrow(allresults),2]
  k<- c(0, nrow(allresults))
  repeat{
    init<- c(x, start)
    samples<-M.MSH.sampler(LB,init, post, sigma, log)
    y<-x
    x<- samples[nrow(samples),1]
    z<-matrix(c(rep(NA, (nrow(allresults))*length(init)-length(init)),init), ncol=ncol(samples), byrow=TRUE)
    z1<-matrix(rep(NA, (ncol(allresults))*nrow(samples)), ncol=ncol(allresults), byrow=TRUE)
    allresults<- cbind(allresults,z)
    sample<- cbind(z1,samples)
    allresults<- rbind(allresults,sample)
    k<- c(k, nrow(allresults))
    if(length(unique(samples[nrow(samples),]))==1) break
  }
  return(y) }
}

