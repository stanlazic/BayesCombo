#' @title Calculate Bayes factors for the unconstrained model
#'
#' @description Using the unconstained prior and posterior models, the unconstrained Bayes factors for each
#'  of the hypotheses are calculated.
#'
#' @details The unconstrained Bayes factors are calculated using proportions calculated by sampling
#'  the distributions of the unconstrained prior and posterior models. This function calls upon a sampling function sample.prop.
#'
#' @param BFcombo An object of class BFcombo
#' @param n Number of times the distributions are sampled
#'
#' @return Object of class BFcombo which contains the values for the unconstrained Bayes factors for each hypothesis.
#' @seealso \code{\link{sample.prop}}
#'
#' @examples
#' x <- prior.var( beta1 = c(0.090,0.140,1.090,1.781),
#'      var1 = c(0.000841,0.002916,0.008649,0.032041),
#'      beta0 = 0, pi0 = rep(1/3,3))
#' x <- u.post.param(x)
#' x <- unconstrained.BF(x,1000)


unconstrained.BF<- function(BFcombo,n = 1000){
  if(any(names(BFcombo) == "unBetaPost")){
    #BF_0u NULL
    n.studies<- length(BFcombo$unBetaPost)
    name.vec<- rep(NA,n.studies)
    for(i in 1:n.studies){
      name.vec[i]<- paste0("study",i)
    }
    BF.0u<- rep(0,n.studies)
    for( i in 1:n.studies){
      BF.0u[i]<- dnorm(BFcombo$beta0,BFcombo$unBetaPost[i],sqrt(BFcombo$unVarPost)[i])/dnorm(BFcombo$beta0,BFcombo$beta0,sqrt(BFcombo$var0)[i])
    }

    ## Run sampler for BF.1u
    temp.proportions<- sample.prop(n,BFcombo$beta0,BFcombo$unBetaPost,sqrt(BFcombo$var0),sqrt(BFcombo$unVarPost),hypothesis = 1)
    # BF_1u in the case m = >


    BF.1u<- temp.proportions[1,] /  temp.proportions[2,]

    # BF.2u in the case m = <

    BF.2u<- (1 - temp.proportions[1,]) / temp.proportions[2,]


    out<- rbind(BF.0u,BF.1u,BF.2u)
    rownames(out)<- c("H:0","H:>","H:<")
    colnames(out)<- name.vec
    BFcombo$BFmu<- out
    return(BFcombo)
  } else{
    if(class(BFcombo) == "BFcombo"){
      stop("Sections unBetaPost and unVarPost not present, please run u.post.params function")
    }else{
      stop("Object not of BFcombo class, please see make.BFcombo function")
    }
  }

}
