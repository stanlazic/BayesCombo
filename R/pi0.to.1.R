#' @title Calculate the posterior model probabilities given the prior model
#' probabilities and the unconstrained Bayes factors.
#'
#'@description Calculates the posterior model probabilities given the prior
#' model probabilities and the unconstrained Bayes factors, this is then updated
#' by setting the posterior model probabilities to the prior model probabilities.
#'
#' @details This function is called from within the update.PMP function and is
#' not called by the user.
#'
#' @param BF Vector of Bayes factors which correspond to the prior model
#' probabilities.
#' @param pi0 Vector of prior model probabilities.
#'
#' @seealso \code{\link{PMP.update}}


pi0.to.1 <- function(pi0, BF) {
    # Pi0 are the prior model probabilities and
    # BF are the Bayes factors BF.mu for a single study
    out <- (pi0 * BF) / sum(pi0 * BF)
    out
}
