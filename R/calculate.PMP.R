#' @title Complete Posterior Model Proababilities function.
#' @description Wrapper function to caclulate the posterior model probabilities from the observed means and variances. 
#'  
#' @details Creates the BFcombo object from the inputs and runs the correct functions in the correct order to produce the posterior model
#' probabilities. 
#'
#'
#'@param beta1
#'
#' @return Object of class BFcombo which contains a matrix of posterior model probabilities for each updated step.
#' @seealso \code{\link{pi0.to.1}}
#'
#' @examples
#' x <- PMP.func( beta1 = c(0.090,0.140,1.090,1.781), var1 = c(0.000841,0.002916,0.008649,0.032041), beta0 = 0, pi0 = rep(1/3,3) )
#' 

calculate.PMP <- function(beta1, var1,pi0,beta0, n = 10000, var.mult = 1){
  BFcombo <- make.BFCombo(beta1 = beta1, var1 = var1, beta0 = beta0, pi0 = pi0)
  BFcombo <- prior.var(BFcombo)
  
  BFcombo$var0 <- BFcombo$var0*var.mult
  
  BFcombo <- u.post.param(BFcombo)

  BFcombo <- unconstrained.BF(BFcombo,n)
  
  PMP <- update.PMP(BFcombo)
  
  PMP
}