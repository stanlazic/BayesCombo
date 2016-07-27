#' @title Plot of Posterior model probabilities
#'
#' @description Produces a visualisation of posteriror model probabilities by plotting the probabilities at each updating step.
#'
#' @details There is a line for each hypothesis and the starting point is the prior model probabilities for each
#' hypothesis.
#'
#' @param BFcombo A object of the class BFcombo which contains the PMP section
#' @param pi0 Vector of prior model probabilities
#'
#' @return Plot of posterior model probabilities.
#'
#' @seealso \code{\link{calculate.PMP}}
#'
#' @export
#'
#' @examples
#' x <- calculate.PMP(beta1 = c(0.0126474408, 5.0051724138, 1.2975612498, 0.0004762455),
#'        var1 = c(2.538974e-03, 6.662216e+00, 4.219142e+00, 6.963380e-06),
#'     beta0 = 0, pi0 = rep(1/3,3) )
#' PMP.plot(x)

PMP.plot <- function(BFcombo, pi0 = rep(1 / 3, 3)) {
    n.studies <- ncol(BFcombo$PMP)
    BFcombo$PMP <- cbind(pi0, BFcombo$PMP)
    plot(x = 0:n.studies, y = BFcombo$PMP[1, ], type = "l", ylim = c(0, 1),
         col = "red", ylab = "Probability", xlab = "PMP",
         main = "Plot of PMP probabilities and initial PrMP")
    lines(0:n.studies, BFcombo$PMP[2, ], col = "blue")
    lines(0:n.studies, BFcombo$PMP[3, ], col = "green")
    legend("topleft", legend = c("H:0", "H:>", "H:<"),
           fill = c("red", "blue", "green"))
}
