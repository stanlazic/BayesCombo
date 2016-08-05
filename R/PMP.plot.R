#' @title Plot of posterior model probabilities
#'
#' @description Plots the updated posterior model probabilities as each study
#' is added to the total.
#'
#' @details Each line is a hypothesis and they start at the prior model
#' probabilities (usually 1/3).
#'
#' @param BFcombo A object of the class BFcombo which contains the PMP section.
#' @param pi0 Vector of prior model probabilities.
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

PMP.plot <- function(BFcombo) {

    n.studies <- ncol(BFcombo$PMP)
    BFcombo$PMP <- cbind(BFcombo$pi0, BFcombo$PMP)

    plot(x = 0:n.studies, y = BFcombo$PMP[1, ], type = "l", ylim = c(0, 1),
         col = "red", ylab = "Probability", xlab = "PMP",
         main = "Plot of PMP probabilities and initial PrMP")

    lines(0:n.studies, BFcombo$PMP[2, ], col = "blue")
    lines(0:n.studies, BFcombo$PMP[3, ], col = "green")

    legend("topleft", legend = c("H:0", "H:>", "H:<"),
           fill = c("red", "blue", "green"))
}
