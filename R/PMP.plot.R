#' @title Plot of Posterior model probabilities
#'
#' @description Produces a visualisation of posteriror model probabilities by plotting the probabilities at each updating step.
#'
#' @details There is a line for each hypothesis and the starting point is the prior model probabilities for each
#' hypothesis.
#'
#' @param BFcombo A object of the class BFcombo which contains the PMP section
#'
#' @return Plot of posterior model probabilities.
#'
#' @seealso \code{\link{calculate.PMP}}
#'
#' @examples
#' x <- calculate.PMP( beta1 = c(0.090,0.140,1.090,1.781), var1 = c(0.000841,0.002916,0.008649,0.032041), beta0 = 0, pi0 = rep(1/3,3) )
#' PMP.plot(x)

PMP.plot<- function(BFcombo,pi0 = rep(1/3,3)){
  n.studies <- ncol(BFcombo$PMP)
  BFcombo$PMP<- cbind(pi0,BFcombo$PMP)
  plot(x = 0:n.studies,y = BFcombo$PMP[1,], type = "l", ylim = c(0,1),col = "red", ylab = "Probability", xlab = "PMP",
       main = "Plot of PMP probabilities and initial PrMP")
  lines(0:n.studies,BFcombo$PMP[2,], col = "blue")
  lines(0:n.studies,BFcombo$PMP[3,], col = "green")
  legend("topleft", legend = c("H:0","H:>","H:<"), fill = c("red","blue","green"))
}
