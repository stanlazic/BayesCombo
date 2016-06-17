#' @title Calculate Posterior Model Proababilities.
#' @description Using the unconstrained Bayes factors, the posterior model probabilities can be calculated
#'  
#' @details Using the unconstrained Bayes factors, the posterior model probabilities can be calculated. 
#' These are then set as the prior model porbablities. This is an updating step and will allow the combination of several studies. 
#' Once the prior model probabilities are updated, the posterior model probabilitiesare calcuated again. 
#'
#'
#'@param BFcombo An object of class BFcombo.
#'
#' @return Object of class BFcombo which contains a matrix of posterior model probabilities for each updated step.
#' @seealso \code{\link{pi0.to.1}}
#'
#' @examples
#' x <- make_BFcombo( beta1 = c(0.090,0.140,1.090,1.781), var1 = c(0.000841,0.002916,0.008649,0.032041), beta0 = 0, pi0 = rep(1/3,3) )
#' x <- PriorVar(x)
#' x <- u.post.params(x)
#' x <- BF.mu.func(x)
#' x<- pi.0m.t.func(x)
#'


update.PMP<- function(BFcombo){
  pmp.t<- BFcombo$BFmu
  bfcols<- ncol(BFcombo$BFmu)
  pmp.t[,1]<- pi0.to.1(BFcombo$pi0,BFcombo$BFmu[,1])
  
  for(i in 2:bfcols){
    pmp.t[,i]<- pi0.to.1(pmp.t[,i-1],BFcombo$BFmu[,i])
  }
  names.vec<- rep(0, bfcols)
  for(i in 1:bfcols){
    names.vec[i]<- paste0("PMP", i )
  }
  colnames(pmp.t)<- names.vec
  BFcombo$PMP<- pmp.t
  return(BFcombo)
}

#' @rdname  This function calculates the posterior model probabilities given the prior model probabilities
#' and the unconstrained Bayes factors. 
#' 
#' @param pi0 Vector of prior model probabilities.
#' @param BF Vector of Bayes factors which correspond to the prior model probabilities.
 

pi0.to.1<- function(pi0,BF){
  # Pi0 are the prior Model Probabilities and BF are the Bayes factors BF.mu for a single study
  # Careful not to use the whole matrix of BF's here !
  
  out<- (pi0*BF) / sum(pi0*BF)
  out
}