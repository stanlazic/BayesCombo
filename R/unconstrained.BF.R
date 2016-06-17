#' @title Calculate the Bayes factors for the unconstrained model.
#' @description Using the unconstained prior and posterior models, the unconstrained Bayes factors for each
#'  of the hypotheses are calculated.
#'  
#' @details The unconstrained Bayes factors are calculated using proportions calculated by sampling
#'  the distributions of the unconstrained prior and posterior models. This function calls upon a sampling function sampleProp.
#'  
#'
#'
#'@param BFcombo an object of class BFcombo.
#'@param n Number of times the distributions are sampled.
#'
#' @return Object of class BFcombo which contains the values for the unconstrained Bayes factors for each hypothesis.
#' @seealso \code{\link{sampleProp}}
#'
#' @examples
#' x <- make_BFcombo( beta1 = c(0.090,0.140,1.090,1.781), var1 = c(0.000841,0.002916,0.008649,0.032041), beta0 = 0, pi0 = rep(1/3,3) )
#' x <- PriorVar(x)
#' x <- u.post.params(x)
#' x <- BF.mu.func(x)
#'

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


#' @rdname  Gives the sampling proportions for the unconstrained parameters and is called in the BF.mu.func function.
#' 
#' @param n Number of times the distributions are sampled.
#' @param prior.mean Vector of unconstrained prior  model means. This is part of the BFcombo object.
#' @param posterior.mean Vector of unconstrained posterior model means. This is part of the BFcombo object.
#' @param prior.sd Vector of unconstrained prior model standard devidations. The variances are part of the BFcombo object.
#' @param prior.mean Vector of unconstrained prior model standard devidations, The variances are part of the BFcombo object.
#' 
sample.prop<- function(n,prior.mean,posterior.mean,prior.sd,posterior.sd,hypothesis = 1){
  if(hypothesis == "1"){
    "%a%" = function(x,y){x > y}
  } else {
    "%a%" = function(x,y) {
      x < y
    }
  }
  
  n.post<- length(posterior.mean)
  Cm<- Fm<- names.vec<- rep(0,n.post)
  
  Cm[]<- 0.5
  
  for(i in 1:n.post){
    Fm[i]<- mean(ifelse(rnorm(n,posterior.mean[i],posterior.sd[i]) %a% prior.mean , 1 , 0))
    
    names.vec[i]<- paste("prob.post",i,sep = "")
    
  }
  
  out<- matrix(0,ncol = (n.post), nrow = 2)
  rownames(out)<- c("Fm","Cm")
  colnames(out)<- names.vec
  out[2,]<- Cm
  out[1,]<- Fm
  out
}
