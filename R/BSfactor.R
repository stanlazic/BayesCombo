#' @title Function for Bayes Safety Factor
#'
#' @description How much can the prior model probability be weighted to the null hypothesis.
#'
#' @details Breaks the weights from the probability being 1/3 for each hypothesis to incrementally give the
#' null hypothisis more weight.
#'
#'
#' @param BFcombo. A object of the class BFcombo (see make_BFcombo). *** NEED TO CHANGE **
#'
#' @return .
#' @seealso \code{\link{make_BFcombo}}
#'
#' @examples
#' x <- make_BFcombo( beta1 = c(0.090,0.140,1.090,1.781), var1 = c(0.000841,0.002916,0.008649,0.032041), beta0 = 0, pi0 = rep(1/3,3) )
#'
BSfactor <- function(beta1 = c(0.090,0.140,1.090,1.781), var1 = c(0.000841,0.002916,0.008649,0.032041), beta0 = 0, n = 100, hypothesis = 1){

  values <- seq(0.33333, 0.99999,length.out = n)
  priors <- cbind( "H=0"=values,"H:>"=(1-values)/2, "H:<"=(1-values)/2)
  count<- 1
  tot.len<- length(priors[,1])
  stpr<- 0
  studies<- length(beta1)
  output<-priors
  output[]<- 0

  for(i in 1:tot.len){

    temp<- calculate.PMP(beta1, var1, beta0, pi0 = priors[i,])
    output[i,]<-temp$PMP[,studies]

  }

  if(hypothesis == 1 & output[1,2] < 0.95){
    warning("The probability for the hypothesis H:> is not greater than 0.95 at the first step")
  }

  if(hypothesis == 2 & output[1,3] < 0.95){
    warning("The probability for the hypothesis H:< is not greater than 0.95 at the first step")
  }

  return(list(PMP = output, priorMP = priors ))
}
