#' @title Plot of prior, data, and posterior distributions
#'
#' @description Plots the results from a single study.
#'
#' @details Plots the prior, data (likelihood), and posterior distribution
#' calculated from the \code{pmp()} function for a single study.
#'
#' @param x A \code{PMP} object created by the \code{pmp()} function.
#' 
#' @param range A numeric vector of length two that sets the range of the
#' x-axis. Values are passed to \code{xlim}. Sensible default values are used
#' when \code{range = NULL}.
#' 
#' @param n An integer that specifies the number of x-values to use when
#' plotting the distributions.
#' 
#' @param leg.loc Location of the legend; default is top left. See the
#' \code{legend()} help pages for all the options. If \code{leg.loc = NULL} the
#' legend is not plotted.
#'
#' @param xlab,ylab Text for the x and y axes labels.
#' 
#' @param ...  Other options passed to \code{plot()}.
#'
#' @return Plot of distributions.
#'
#' @seealso \code{\link{pmp}}
#'
#' @export
#'
#' @examples
#' x <- pmp(beta = 5.005, se.beta = 2.05)
#' plot(x)

plot.PMP <- function(x, range=NULL, n=200, leg.loc="topleft",
                     xlab="Effect size", ylab="", ...) {

    if (!inherits(x, "PMP") ) {
        stop("Input must be a 'PMP' object from the pmp() function.")
    }

    if (is.null(range)){
        xvals <- seq(x$beta0 - 3*x$se0, x$beta0 + 3*x$se0, length.out = n)
    } else {
        xvals <- seq(range[1], range[2], length.out = n)
    }

    lik <- dnorm(xvals, x$beta, x$se.beta)
    prior <- dnorm(xvals, x$beta0, x$se0)
    post <- dnorm(xvals, x$post.b, x$post.se)

    plot(prior ~ xvals, col="darkgrey", lwd=3, type="l",
         ylim=c(0, max(c(lik, prior, post))), xlim=c(min(xvals), max(xvals)),
         xlab = xlab, ylab = ylab, ...)
    lines(lik ~ xvals, lty=2)
    lines(post ~ xvals)

    if (!is.null(leg.loc)){
        legend(leg.loc, legend=c("Prior", "Data", "Posterior"),
               lwd=c(3, 1, 1), lty=c(1, 2, 1),
               col=c("darkgrey", "black", "black"))
    }
}
