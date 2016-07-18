#' @title Calculate the prior variances for the unconstrained model
#' @description Given a set of values for the observed mean,variance and the prior mean, a set of unconstrained prior varuiances are calculated.
#'
#' @details The prior varianes are calculated by getting the largest confidence interval from the prior mean by using the
#' confidence interval of the observed values.
#'
#'
#' @param beta1 Vector of observed means
#' @param var1 Vector of observed variances
#' @param pi0 Vector of prior model probabilities
#' @param beta0 Vector or value of prior means (usually 0)
#' @param percent Confidence interval precentage
#'
#' @return vector of equal length to that of the observed variances.
#'

prior.var<- function(beta1, var1, beta0, pi0, percent = 99){


  if(is.numeric(beta1) != TRUE){
    stop("beta1 is not numeric")
  }

  if(is.numeric(var1) != TRUE){
    stop("var1 is not numeric")
  }

  if(is.numeric(beta0) != TRUE){
    stop("beta0 is not numeric")
  }

  if(is.numeric(pi0) != TRUE){
    stop("pi0 is not numeric")
  } else if (sum(pi0) != 1){
    stop("Prior model probabilities do not sum to 1")
  }

  BFcombo<- structure(list(beta1 = beta1, var1 = var1,
                                      beta0 = beta0, pi0 = pi0 ), class = "BFcombo")
    #setup
    percent<- (percent / 100)
    percent<- percent + (1-percent)/2
    multiplier<- qnorm(percent)
    data<- matrix(0, ncol = 2 , nrow = length(BFcombo$beta1))
    # get min/max
    data[,1]<- BFcombo$beta1 - sqrt(BFcombo$var1)*multiplier
    data[,2]<- BFcombo$beta1 + sqrt(BFcombo$var1)*multiplier

    bounds<- abs(data)
    pVar <- apply(X = bounds,MARGIN = 1,max)
    pVar <- ((pVar / multiplier))^2
    # update.object
    BFcombo$var0 <- pVar

    return(BFcombo)

}

