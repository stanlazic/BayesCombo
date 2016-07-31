#' @title Plots Bayesian safety (BS) factor results
#'
#' @description Produces a visualisation of the Bayes safety factor results.
#'
#' @details Each solid line is a hypothesis, the x-axis is the prior model
#' probability for the null hypothesis and the y-axis is the posterior model
#' probability. The dotted line is the threshold.
#'
#' @param x object of the class BSfactor
#' @param ... additional parameters not required
#'
#' @return Plot of posterior model probabilities in relation to changes in the
#' prior model probabilities.
#' 
#' @seealso \code{\link{BSfactor}}
#' 
#' @importFrom utils tail
#' @export
#' @examples
#' x <- BSfactor( beta = c(0.0126474408, 5.0051724138, 1.2975612498, 0.0004762455),
#'        se.beta = c(2.538974e-03, 6.662216e+00, 4.219142e+00, 6.963380e-06),
#'        beta0 = 0, reverse = TRUE )
#' plot(x)

plot.BSfactor <- function(x, ...) {
    namevec <- c("H:0", "H:>", "H:<")
    
    plot(x = x$priorMP[, 1], y = x$PMP[, 1], type = "l",
         ylim = c(0, 1), col = "red",
         ylab = "Posterior model probability",
         xlab = paste(namevec[tail(x$priorMP, 1) == max(tail(x$priorMP, 1))],
         "prior model probability"),
         main = "Plot of posterior model probabilities
         vs changing prior model probabilities")
    
    lines(x$priorMP[, 1], x$PMP[, 2], col = "blue")
    lines(x$priorMP[, 1], x$PMP[, 3], col = "green")
    abline(h = x$threshold, lty = 2)
    
    legend("left", legend = c("H:0", "H:>", "H:<"),
           fill = c("red", "blue", "green"))
}
