#' @title Plot of posterior model probabilities
#'
#' @description Plots the updated posterior model probabilities as each study
#' is added to the total.
#'
#' @details Each line is a hypothesis and they start at the prior model
#' probabilities (usually 1/3).
#'
#' @param BFcombo A object of the class BFcombo.
#' @param ... Other options to pass to \code{plot()}.
#'
#' @return Plot of posterior model probabilities.
#'
#' @seealso \code{\link{calculate.PMP}}
#'
#' @export
#'
#' @examples
#' x <- calculate.PMP( beta = c(0.0126, 5.0052, 1.2976, 0.0005),
#'        se.beta = c(0.050, 2.581, 2.054, 0.003) )
#' PMP.plot(x)

PMP.plot <- function(BFcombo, ...) {

  BFcombo$PMP <- cbind(BFcombo$pi0, BFcombo$PMP)
  n<- length(BFcombo$PMP[1,])
  matplot(t(BFcombo$PMP), type="l", col=c("red","green","blue"), lty=1,
          ylim = c(0, 1), ylab = "Probability", xlab = "Study", xaxt = "n",
          ...)
  axis(1, at = 1:n, labels = 0:(n-1))
  legend("topleft", legend = c("H:0", "H:>", "H:<"),
         fill = c("red", "blue", "green"))
}
