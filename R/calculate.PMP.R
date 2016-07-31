#' @title Calculate posterior model probabilities
#'
#' @description Calculate the posterior model probabilities from the observed
#' effect sizes and their standard errors.
#'
#' @details Creates the BFcombo object from the inputs and of posterior model
#' probabilities given the inputs.
#'
#' @param beta Vector of observed effect sizes.
#' @param se.beta Vector of observed standard errors for the effect sizes.
#' @param pi0 Vector of prior model probabilities specified in the following
#' order: null, greater than zero, less than zero. The priors default to 1/3 for
#' each.
#' @param beta0 Vector or value of prior effect sizes (usually 0).
#' @param n Number of times distributions are sampled.
#' @param var.mult Multiplier for the prior variances. Can be used to test the
#' sensitivity of the results to the width of the prior.
#' @param percent Sets the percentage of the confidence interval. Default is
#' 99%.
#'
#' @return Object of class BFcombo which contains a matrix of posterior model
#' probabilities for each updated step.
#' 
#' @seealso \code{\link{prior.var} \link{u.post.param} \link{unconstrained.BF}
#' \link{PMP.update}}
#'
#' @export
#' @examples
#' x <- calculate.PMP(beta = c(0.0126474408, 5.0051724138, 1.2975612498, 0.0004762455),
#'        se.beta = c(2.538974e-03, 6.662216e+00, 4.219142e+00, 6.963380e-06),
#'     beta0 = 0, pi0 = rep(1/3,3) )
#'
#' summary(x)


calculate.PMP <- function(beta, se.beta, pi0 = rep(1 / 3, 3), beta0 = 0,
                            n = 10000, var.mult = 1, percent = 99) {
    BFcombo <- prior.var(beta = beta, se.beta = se.beta,
                         beta0 = beta0, pi0 = pi0, 99)
    BFcombo$percent <- percent
    BFcombo$var0 <- BFcombo$var0 * var.mult
    BFcombo <- u.post.param(BFcombo)
    BFcombo <- unconstrained.BF(BFcombo, n)
    PMP <- PMP.update(BFcombo)

    return(PMP)
}
