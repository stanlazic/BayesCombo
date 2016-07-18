#' @title Updating Posterior Model Proababilities
#'
#' @description The posterior model probabilities can be calculated using the unconstrained Bayes factors and then updated.
#'
#' @details
#' The posterior model probabilities are then set as the prior model porbablities. This is an updating step and will allow the combination of several studies.
#' Once the prior model probabilities are updated, the posterior model probabilitiesare calcuated again.
#'
#' @param BFcombo An object of class BFcombo
#'
#' @return Object of class BFcombo which contains a matrix of posterior model probabilities for each updated step.
#' @seealso \code{\link{pi0.to.1} \link{calculate.PMP}}
#'

PMP.update<- function(BFcombo){
  if(any(names(BFcombo) == "BFmu")){
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
  } else{
    if(class(BFcombo) == "BFcombo"){
      stop("no BFmu section in the input, please run unconstrained.BF function")

    }else{
      stop("Object not of BFcombo class, please see make.BFcombo function")
    }
  }

}

