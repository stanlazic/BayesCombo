#' @title Calculate Bayes Safety factor
#'
#' @description Calculates how much can the prior model probability be weighted to the null hypothesis.
#'
#' @details Incrementally increases a the prior posterior probability for a given hypothesis, and shows the resulting posterior
#' model probabilities
#'
#' @param beta1 Vector of observed means
#' @param var1 Vector of observed variances
#' @param threshold Sets the value at which the border is set
#' @param beta0 Vector or value of prior means. Default is  0
#' @param n Number of steps between 0.33333 and 0.99999
#' @param hypothesis Parameter has different functionalities depending on value of reverse
#' @param reverse Changes the hypothesis in which the prior model probability increases. If this is TRUE then the value of the hypothesis parameter will determine the
#' hypothesis which will increase in probability
#'
#'
#' @return Object of class "BSfactor" which contains a matrix of final posterior model probabilities, a matrix of prior model probabilities,
#' the 'boundary' and which hypothesis is being considered
#'
#' @export
#' @examples
#'
#' x <- BSfactor( beta1 = c(0.068,-0.084,0.175,0.337),
#'      var1 = c(0.000841,0.002916,0.008649,0.032041),
#'      beta0 = 0 )
#'
#' @seealso \code{\link{plot.BSfactor}, \link{seq}}
#'


BSfactor<- function(beta1, var1, beta0 = 0, n = 100, threshold = 0.95, hypothesis = 1, reverse = FALSE){

  values <- seq(0.33333, 0.99999,length.out = n)

  if (reverse == FALSE){
    priors <- cbind( "H=0"=values,"H:>"=(1-values)/2, "H:<"=(1-values)/2)
    tot.len <- length(priors[,1])
    studies <- length(beta1)
    output <-priors
    output[]<- 0

    for(i in 1:tot.len){

      temp <- calculate.PMP(beta1, var1, beta0, pi0 = priors[i,])
      output[i,] <- temp$PMP[,studies]

    }


    if(hypothesis == 1 & output[1,2] < threshold){
      warning("The probability for the hypothesis H:> is not greater than threshold at the first step")
    }

    if(hypothesis == 2 & output[1,3] < threshold){
      warning("The probability for the hypothesis H:< is not greater than threshold at the first step")
    } else if(sum(round(output[,1+hypothesis], digits = 2) == threshold) > 0){
      boundary<- priors[round(output[,1+hypothesis], digits = 2) == threshold,]
      if(sum(round(output[,1+hypothesis], digits = 2) == threshold) > 1 ){
        boundary<- boundary[1,]
      }
    } else{
      warning("Not rounding to threshold , boundary = NULL")
      boundary<- NULL
    }
    return( structure(list(PMP = output, priorMP = priors, boundary = boundary, hypothesis = hypothesis, threshold = threshold ),class = "BSfactor"))
  }else{

    if(hypothesis == 1){

      priors <- cbind( "H=0"= (1-values)/2, "H:>"= values, "H:<"=(1-values)/2)
    }

    if(hypothesis == 2){

      priors <- cbind( "H=0"= (1-values)/2, "H:>"= (1-values)/2, "H:<"= values)
    }

    tot.len<- length(priors[,1])
    studies<- length(beta1)
    output<-priors
    output[]<- 0

    for(i in 1:tot.len){

      temp<- calculate.PMP(beta1, var1, beta0, pi0 = priors[i,])
      output[i,]<-temp$PMP[,studies]

    }

    if(sum(round(output[,1+hypothesis], digits = 2) == threshold) > 0){
      boundary<- priors[round(output[,1+hypothesis], digits = 2) == threshold,]
      if(sum(round(output[,1+hypothesis], digits = 2) == threshold) > 1 ){
        boundary<- boundary[1,]
      }
    } else{
      warning("Not rounding to threshold , boundary = NULL")
      boundary<- NULL
    }
    return( structure(list(PMP = output, priorMP = priors, boundary = boundary,threshold = threshold , hypothesis = hypothesis),class = "BSfactor"))
  }
}


