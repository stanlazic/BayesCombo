#' @title Plot of Bayes Safety factor results
#'
#' @description Produces a visualisation of the Bayes safety factor results
#'
#' @details There is a line for each hypothesis, the x-axis is of the prior model proability for the null hypothesis,
#' the y-axis is the posterior model probability.
#'
#'
#' @param BSfactor A object of the class BSfactor.
#'
#' @return Plot of posterior model probabilities in relation to changes in the prior model probabilities.
#' @seealso \code{\link{calculate.PMP}}
#'
#' @examples
#'
#' x <- calculate.PMP( beta1 = c(0.090,0.140,1.090,1.781), var1 = c(0.000841,0.002916,0.008649,0.032041), beta0 = 0, pi0 = rep(1/3,3) )
#' plot(x)

plot.BSfactor<- function(BSfactor,threshold = 0.95){
  n.prior<- nrow(BSfactor$priorMP)
  plot(x = BSfactor$priorMP[,1],y = BSfactor$PMP[,1], type = "l", ylim = c(0,1),col = "red", ylab = "Probability", xlab = "H:0 PriorMP",
       main = "Plot of posterior model probabilities vs changing prior model probabilities")
  lines(BSfactor$priorMP[,1],BSfactor$PMP[,2], col = "blue")
  lines(BSfactor$priorMP[,1],BSfactor$PMP[,3], col = "green")
  abline(h = threshold,lty = 2)
  legend("left", legend = c("H:0","H:>","H:<"), fill = c("red","blue","green"))
}
