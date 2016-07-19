#' @title Summarize a BFcombo object
#'
#' @description Gives summary of various sections of the BFcombo class.
#'
#' @details For all the sections which provide a mean or variance vector, the summary function will return a minimum and maximim value.
#' For a Bayes factor or probability, the summary function will return a matrix with the values rounded to 4 decimal points.
#'
#' @param object A object of the class BFcombo
#' @param ... Additional parameters
#'
#' @return A summary of the BFcombo object passed.
#'
#' @seealso \code{ \link{calculate.PMP} \link{prior.var}}
#'
#' @export
#' @examples
#' x <- calculate.PMP( beta1 = c(0.0126474408, 5.0051724138, 1.2975612498, 0.0004762455),
#'     var1 = c(2.538974e-03, 6.662216e+00, 4.219142e+00, 6.963380e-06),
#'     beta0 = 0, pi0 = rep(1/3,3) )
#' summary(x)


summary.BFcombo<- function(object, ...){
  # get categories
  categ<- names(object)

  if (all( categ != "var1")){
    output<- list(Observed = NULL)
    warning("Only object inputs included")

    Min_Obs<- round(c(min(object$beta1),min(object$var1)),digits = 4)
    Max_Obs<- round(c(max(object$beta1),max(object$var1)),digits = 4)

    output$Observed<- data.frame(Min = Min_Obs , Max = Max_Obs)
    rownames(output$Observed)<- c("Mean","Variance")
  } else if (all( categ != "unBetaPost")) {

    output<- list(Observed = NULL, uPrior = NULL)

    Min_Obs<- round(c(min(object$beta1),min(object$var1)),digits = 4)
    Max_Obs<- round(c(max(object$beta1),max(object$var1)),digits = 4)

    output$Observed<- data.frame(Min = Min_Obs , Max = Max_Obs)
    rownames(output$Observed)<- c("Mean","Variance")

    ###


    Min_uPrior<- round(c(min(object$beta0),min(object$var0)),digits = 4)
    Max_uVariance<- round(c(max(object$beta0),max(object$var0)),digits = 4)

    output$Observed<- data.frame(Min = Min_Obs , Max = Max_Obs)
    rownames(output$Observed)<- c("Mean","Variance")

    output$uPrior<- data.frame(Min = Min_uPrior , Max = Max_uVariance)
    rownames(output$uPrior)<- c("Mean","Variance")

  } else if (all( categ != "BFmu")){
    output<- list(Observed = NULL, uPrior = NULL, uPosterior = NULL)

    Min_Obs<- round(c(min(object$beta1),min(object$var1)),digits = 4)
    Max_Obs<- round(c(max(object$beta1),max(object$var1)),digits = 4)


    Min_uPrior<- round(c(min(object$beta0),min(object$var0)),digits = 4)
    Max_uVariance<- round(c(max(object$beta0),max(object$var0)),digits = 4)


    Min_uPosterior<- round(c(min(object$unBetaPost),min(object$unVarPost)),digits = 4)
    Max_uPostVariance<- round(c(max(object$unBetaPost),max(object$unVarPost)),digits = 4)

    output$Observed<- data.frame(Min = Min_Obs , Max = Max_Obs)
    rownames(output$Observed)<- c("Mean","Variance")

    output$uPrior<- data.frame(Min = Min_uPrior , Max = Max_uVariance)
    rownames(output$uPrior)<- c("Mean","Variance")

    output$uPosterior<- data.frame(Min = Min_uPosterior , Max = Max_uPostVariance)
    rownames(output$uPosterior)<- c("Mean","Variance")

  } else if(all( categ != "PMP")){

    output<- list(Observed = NULL, uPrior = NULL, uPosterior = NULL,BFmu = NULL)

    Min_Obs<- round(c(min(object$beta1),min(object$var1)),digits = 4)
    Max_Obs<- round(c(max(object$beta1),max(object$var1)),digits = 4)


    Min_uPrior<- round(c(min(object$beta0),min(object$var0)),digits = 4)
    Max_uVariance<- round(c(max(object$beta0),max(object$var0)),digits = 4)


    Min_uPosterior<- round(c(min(object$unBetaPost),min(object$unVarPost)),digits = 4)
    Max_uPostVariance<- round(c(max(object$unBetaPost),max(object$unVarPost)),digits = 4)

    output$BFmu <- round(object$BFmu,digits = 4)

    output$Observed<- data.frame(Min = Min_Obs , Max = Max_Obs)
    rownames(output$Observed)<- c("Mean","Variance")

    output$uPrior<- data.frame(Min = Min_uPrior , Max = Max_uVariance)
    rownames(output$uPrior)<- c("Mean","Variance")

    output$uPosterior<- data.frame(Min = Min_uPosterior , Max = Max_uPostVariance)
    rownames(output$uPosterior)<- c("Mean","Variance")


  } else if(any(categ == "PMP")){
    output<- list(Observed = NULL, uPrior = NULL, uPosterior = NULL,BFmu = NULL, PMP = NULL)

    Min_Obs<- round(c(min(object$beta1),min(object$var1)),digits = 4)
    Max_Obs<- round(c(max(object$beta1),max(object$var1)),digits = 4)


    Min_uPrior<- round(c(min(object$beta0),min(object$var0)),digits = 4)
    Max_uVariance<- round(c(max(object$beta0),max(object$var0)),digits = 4)


    Min_uPosterior<- round(c(min(object$unBetaPost),min(object$unVarPost)),digits = 4)
    Max_uPostVariance<- round(c(max(object$unBetaPost),max(object$unVarPost)),digits = 4)

    output$BFmu <- round(object$BFmu,digits = 4)

    output$PMP <- round(object$PMP,digits = 4)

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
