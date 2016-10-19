#' @title Plot of posterior model probabilities
#'
#' @description Plots the updated posterior model probabilities as each study is
#' added.
#'
#' @details Each line is a hypothesis and they start at the prior model
#' probabilities (usually 1/3).
#'
#' @param x An object of the class \code{PMPlist} created by the
#' \code{pmp.update()} function.
#' 
#' @param leg.loc Location of the legend; default is top left. See the
#' \code{legend} help pages for all the options.
#' 
#' @param lty An vecter of integers that specifies the line types. See the
#' \code{matplot()} function for details.
#' 
#' @param ... Other options passed to \code{matplot()}.
#'
#' @return Plot of posterior model probabilities.
#'
#' @seealso \code{\link{pmp.update}}
#'
#' @export
#'
#' @examples
#' x <- pmp.update(beta = c(0.0126, 5.0052, 1.2976, 0.0005),
#'        se.beta = c(0.050, 2.581, 2.054, 0.003) )
#' plot(x)

plot.PMPlist <- function(x, leg.loc="topleft", lty=1, ...) {

    if (!inherits(x, "PMPlist") ) {
        stop("Input must be a 'PMPlist' object from the pmp.update() function.")
    }

    matplot(x$pmps, type="l", col=c("royalblue","darkgrey","firebrick"), lty=lty,
            lwd=c(1,3,1), ylim = c(0, 1), xaxt = "n", ...)
    axis(1, at = 1:(x$N+1), labels = 0:(x$N))

    legend(leg.loc, legend = c("H<", "H0", "H>"),
           fill = c("royalblue","darkgrey","firebrick"))
}
