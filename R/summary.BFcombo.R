#' @title Summary function for the BFcombo Class
#'
#' @description Gives summary of various sections of the BFcombo class.
#'
#' @details For all the sections which provide a mean or variance vector, the summary function will return a minimum and maximim value.
#' For a Bayes factor or probability, the summary function will return a matrix with the values rounded to 4 decimal points.
#'
#' @param BFcombo A object of the class BFcombo but must also incldes a section with unconstrained prior variances.
#'
#' @return A summary of the BFcombo object passed.
#' 
#' @seealso \code{\link{make_BFcombo} \link{PriorVar}}
#'
#' @examples
#' x <- make_BFcombo( beta1 = c(0.090,0.140,1.090,1.781), var1 = c(0.000841,0.002916,0.008649,0.032041), beta0 = 0, pi0 = rep(1/3,3) )
#' x <- PriorVar(x)
#' x <- u.post.params(x)


summary.BFcombo<- function(BFcombo){
  # get categories
  categ<- names(BFcombo)
  ## sections which may not be included are:
  # var0
  # unBetaPost
  # unVarPost
  # BFmu
  # PMP
  if (all( categ != "var1")){
    output<- list(Observed = NULL)
    warning("Only BFcombo inputs included")
    
    Min_Obs<- round(c(min(BFcombo$beta1),min(BFcombo$var1)),digits = 4)
    Max_Obs<- round(c(max(BFcombo$beta1),max(BFcombo$var1)),digits = 4)
    
    output$Observed<- data.frame(Min = Min_Obs , Max = Max_Obs)
    rownames(output$Observed)<- c("Mean","Variance")
  } else if (all( categ != "unBetaPost")) {
    
    output<- list(Observed = NULL, uPrior = NULL)
    
    Min_Obs<- round(c(min(BFcombo$beta1),min(BFcombo$var1)),digits = 4)
    Max_Obs<- round(c(max(BFcombo$beta1),max(BFcombo$var1)),digits = 4)
    
    output$Observed<- data.frame(Min = Min_Obs , Max = Max_Obs)
    rownames(output$Observed)<- c("Mean","Variance")
    
    ###
    
    
    Min_uPrior<- round(c(min(BFcombo$beta0),min(BFcombo$var0)),digits = 4)
    Max_uVariance<- round(c(max(BFcombo$beta0),max(BFcombo$var0)),digits = 4)
    
    output$Observed<- data.frame(Min = Min_Obs , Max = Max_Obs)
    rownames(output$Observed)<- c("Mean","Variance")
    
    output$uPrior<- data.frame(Min = Min_uPrior , Max = Max_uVariance)
    rownames(output$uPrior)<- c("Mean","Variance")
    
  } else if (all( categ != "BFmu")){
    output<- list(Observed = NULL, uPrior = NULL, uPosterior = NULL)
    
    Min_Obs<- round(c(min(BFcombo$beta1),min(BFcombo$var1)),digits = 4)
    Max_Obs<- round(c(max(BFcombo$beta1),max(BFcombo$var1)),digits = 4)
    
    
    Min_uPrior<- round(c(min(BFcombo$beta0),min(BFcombo$var0)),digits = 4)
    Max_uVariance<- round(c(max(BFcombo$beta0),max(BFcombo$var0)),digits = 4)
    
    
    Min_uPosterior<- round(c(min(BFcombo$unBetaPost),min(BFcombo$unVarPost)),digits = 4)
    Max_uPostVariance<- round(c(max(BFcombo$unBetaPost),max(BFcombo$unVarPost)),digits = 4)
    
    output$Observed<- data.frame(Min = Min_Obs , Max = Max_Obs)
    rownames(output$Observed)<- c("Mean","Variance")
    
    output$uPrior<- data.frame(Min = Min_uPrior , Max = Max_uVariance)
    rownames(output$uPrior)<- c("Mean","Variance")
    
    output$uPosterior<- data.frame(Min = Min_uPosterior , Max = Max_uPostVariance)
    rownames(output$uPosterior)<- c("Mean","Variance")
    
  } else if(all( categ != "PMP")){
    
    output<- list(Observed = NULL, uPrior = NULL, uPosterior = NULL,BFmu = NULL)
    
    Min_Obs<- round(c(min(BFcombo$beta1),min(BFcombo$var1)),digits = 4)
    Max_Obs<- round(c(max(BFcombo$beta1),max(BFcombo$var1)),digits = 4)
    
    
    Min_uPrior<- round(c(min(BFcombo$beta0),min(BFcombo$var0)),digits = 4)
    Max_uVariance<- round(c(max(BFcombo$beta0),max(BFcombo$var0)),digits = 4)
    
    
    Min_uPosterior<- round(c(min(BFcombo$unBetaPost),min(BFcombo$unVarPost)),digits = 4)
    Max_uPostVariance<- round(c(max(BFcombo$unBetaPost),max(BFcombo$unVarPost)),digits = 4)
    
    output$BFmu <- round(BFcombo$BFmu,digits = 4)
    
    output$Observed<- data.frame(Min = Min_Obs , Max = Max_Obs)
    rownames(output$Observed)<- c("Mean","Variance")
    
    output$uPrior<- data.frame(Min = Min_uPrior , Max = Max_uVariance)
    rownames(output$uPrior)<- c("Mean","Variance")
    
    output$uPosterior<- data.frame(Min = Min_uPosterior , Max = Max_uPostVariance)
    rownames(output$uPosterior)<- c("Mean","Variance")
    
    
  } else if(any(categ == "PMP")){
    output<- list(Observed = NULL, uPrior = NULL, uPosterior = NULL,BFmu = NULL, PMP = NULL)
    
    Min_Obs<- round(c(min(BFcombo$beta1),min(BFcombo$var1)),digits = 4)
    Max_Obs<- round(c(max(BFcombo$beta1),max(BFcombo$var1)),digits = 4)
    
    
    Min_uPrior<- round(c(min(BFcombo$beta0),min(BFcombo$var0)),digits = 4)
    Max_uVariance<- round(c(max(BFcombo$beta0),max(BFcombo$var0)),digits = 4)
    
    
    Min_uPosterior<- round(c(min(BFcombo$unBetaPost),min(BFcombo$unVarPost)),digits = 4)
    Max_uPostVariance<- round(c(max(BFcombo$unBetaPost),max(BFcombo$unVarPost)),digits = 4)
    
    output$BFmu <- round(BFcombo$BFmu,digits = 4)
    
    output$PMP <- round(BFcombo$PMP,digits = 4)
    
    output$Observed<- data.frame(Min = Min_Obs , Max = Max_Obs)
    rownames(output$Observed)<- c("Mean","Variance")
    
    output$uPrior<- data.frame(Min = Min_uPrior , Max = Max_uVariance)
    rownames(output$uPrior)<- c("Mean","Variance")
    
    output$uPosterior<- data.frame(Min = Min_uPosterior , Max = Max_uPostVariance)
    rownames(output$uPosterior)<- c("Mean","Variance")
    
    
  } else {
    stop("Please use an input of class BFcombo")
  }
  
  output
}