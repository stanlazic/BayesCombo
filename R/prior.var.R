#' @title Calculate the prior variances for the unconstrained model.
#' @description Given a set of values for the observed mean,variance and the prior mean, a set of unconstrained prior varuiances are calculated.
#'
#' @details The prior varianes are calculated by getting the largest confidence interval from the prior mean by using the
#' confidence interval of the observed values.
#'
#'
#'@inheritParams plot.BFcombo
#'
#' @return vector of equal length to that of the observed variances.
#' @seealso \code{\link{make_BFcombo}}
#'
#' @examples
#' x <- make_BFcombo( beta1 = c(0.090,0.140,1.090,1.781), var1 = c(0.000841,0.002916,0.008649,0.032041), beta0 = 0, pi0 = rep(1/3,3) )
#' x<- PriorVar(x)
#'
prior.var<- function(BFcombo,percent = 99){
  if(all.equal(intersect(names(BFcombo),c("beta1", "var1", "beta0", "pi0")), c("beta1", "var1", "beta0", "pi0"))){
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
  } else{
    if(class(BFcombo) == "BFcombo"){
      stop("Input of class BFcombo not in the right form please see function make.BFcombo.")
    } else{
      stop("Input  not of class BFcombo please see function make.BFcombo.")
    }
  }
  
}

#' @rdname This function creates the BFcombo class from the inputs.
#' 
#' @param beta1 Vector of observed means.
#' @param var1 Vector of observed variances.
#' @param beta0 Value of prior mean (usually 0)
#' @param pi0 Vector of prior model probabilities.
#' 
make.BFCombo<- function(beta1, var1, beta0, pi0){
  BFcombo<- structure(list(beta1 = beta1, var1 = var1,
                           beta0 = beta0, pi0 = pi0 ), class = "BFcombo")
}