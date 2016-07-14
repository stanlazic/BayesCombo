#' @title Plot Bayes Safety factor results
#'
#' @description Produces a visualisation of the Bayes safety factor results.
#'
#' @details Each solid line is a hypothesis, the x-axis is of the prior model proability for the null hypothesis,
#' the y-axis is the posterior model probability. The dotted line is the threshold.
#'
#' @param BSfactor A object of the class BSfactor
#' @param threshold Value of the threshold used to set the border, defualt is 0.95
#' @param ... other graphical parameters
#'
#' @return Plot of posterior model probabilities in relation to changes in the prior model probabilities.
#' @seealso \code{\link{BSfactor}}
#'
#' @export
#' @examples
#' x <- BSfactor( beta1 = c(0.068,-0.084,0.175,0.337),
#'        var1 = c(0.000841,0.002916,0.008649,0.032041),
#'        beta0 = 0 )
#' plot(x)

plot.BSfactor<- function(BSfactor,threshold = 0.95, ... ){
  n.prior<- nrow(BSfactor$priorMP)
  plot(x = BSfactor$priorMP[,1],y = BSfactor$PMP[,1], type = "l", ylim = c(0,1),col = "red", ylab = "Probability", xlab = "H:0 PriorMP",
       main = "Plot of posterior model probabilities vs changing prior model probabilities" , ...)
  lines(BSfactor$priorMP[,1],BSfactor$PMP[,2], col = "blue")
  lines(BSfactor$priorMP[,1],BSfactor$PMP[,3], col = "green")
  abline(h = threshold,lty = 2)
  legend("left", legend = c("H:0","H:>","H:<"), fill = c("red","blue","green"))
}
