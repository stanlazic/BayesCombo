#' @title Forest plot
#'
#' @description Plots the effect sizes and standard errors for a set of studies as a standard forest plot. Also shown are the priors for each study.
#'
#' @details This function plots the data from the \code{pmp.update} function. It may be hard to visualise the results if the effect sizes differ. In this case it may be preferable to standardise the effect sizes by setting \code{scale=TRUE} in the  \code{pmp.update} function.
#'
#' @param x An object of the class \code{PMPlist}.
#' 
#' @param range A numeric vector of length two that sets the range of the x-axis. Values are passed to \code{xlim}. Sensible values are used default when \code{range=NULL}.
#'
#' @param xlab,ylab Text for the x and y axes labels.
#' 
#' @param ...  Other options passed to \code{plot()}.
#' 
#' @return A forest plot.
#'
#' @seealso \code{\link{pmp.update}}
#'
#' @export
#'
#' @examples
#' x <- pmp.update( beta = c(0.0126, 5.0052, 1.2976, 0.0005),
#'        se.beta = c(0.050, 2.581, 2.054, 0.003) )
#' forestplot(x)
#'
forestplot <- function(x, range=NULL, xlab="Effect size",
                         ylab="Study", ...) {

    if (!inherits(x, "PMPlist")){
        stop("Input must be a 'PMPlist' object from the pmp.update() function.")
    }

    # multiplier for confidence intervals
    percent <- (x$percent / 100)
    percent <- percent + (1 - percent) / 2
    multiplier <- qnorm(percent)

    # calclate x-axis range
    if (is.null(range)){
        xlim <- c(min(x$beta0 - x$se0 * multiplier),
                  max(x$beta0 + x$se0 * multiplier))
    } else {
        xlim <- c(range[1], range[2])
    }

    # setup plot
    plot(c(1:x$N) ~ x$beta, type="n", yaxt="n", xlab = xlab, ylab = ylab,
         xlim=xlim, ...)
    axis(2, at=1:x$N)

    # priors
    segments(rep(x$beta0, x$N) - x$se0 * multiplier, 1:x$N,
             rep(x$beta0, x$N) + x$se0 * multiplier, 1:x$N,
             lwd=6, col="darkgrey")

    # data
    arrows(x$beta - x$se.beta * multiplier, 1:x$N,
           x$beta + x$se.beta * multiplier, 1:x$N,
           code=3, angle=90, length=0.05)

    points(c(1:x$N) ~ x$beta, pch=18, cex=1.2)
}
