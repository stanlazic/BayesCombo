#' @title Create object of class 'BFcombo'
#'
#' @description Many of the BayesCombo functions require objects of the class 'BFcombo' to run.
#'
#' @details This function is called within the prior.var function.
#'
#' @param beta1 Vector of observed means
#' @param var1 Vector of observed variances
#' @param beta0 Value of prior mean (usually 0)
#' @param pi0 Vector of prior model probabilities
#'
#' @seealso \code{\link{prior.var}}

make.BFcombo<- function(beta1, var1, beta0, pi0){
  BFcombo<- structure(list(beta1 = beta1, var1 = var1,
                           beta0 = beta0, pi0 = pi0 ), class = "BFcombo")
  BFcombo

}
