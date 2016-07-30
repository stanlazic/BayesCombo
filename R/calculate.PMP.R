#' @title Calculate updated Posterior Model Proababilities
#'
#' @description Wrapper function to caclulate the posterior model probabilities from the observed effect sizes and their respective variances.
#'
#' @details Creates the BFcombo object from the inputs and runs a pipeline to produce the complete set of posterior model
#' probabilities given the inputs.
#'
#' @param beta1 Vector of observed means
#' @param var1 Vector of observed variances
#' @param pi0 Vector of prior model probabilities
#' @param beta0 Vector or value of prior means
#' @param n Number of times distributions are sampled
#' @param var.mult Multiplier of the prior variances
#' @param percent Sets the percentage of the confidence interval. Default is 99.
#'
#' @return Object of class BFcombo which contains a matrix of posterior model probabilities for each updated step.
#' @seealso \code{\link{prior.var} \link{u.post.param} \link{unconstrained.BF} \link{PMP.update}}
#'
#' @export
#' @examples
#' x <- calculate.PMP(beta1 = c(0.0126474408, 5.0051724138, 1.2975612498, 0.0004762455),
#'        var1 = c(2.538974e-03, 6.662216e+00, 4.219142e+00, 6.963380e-06),
#'     beta0 = 0, pi0 = rep(1/3,3) )


calculate.PMP <- function(beta1, var1, pi0 = rep(1 / 3, 3), beta0 = 0,
                            n = 10000, var.mult = 1, percent = 99) {
    BFcombo <- prior.var(beta1 = beta1, var1 = var1,
                         beta0 = beta0, pi0 = pi0, 99)
    BFcombo$percent <- percent
    BFcombo$var0 <- BFcombo$var0 * var.mult
    BFcombo <- u.post.param(BFcombo)
    BFcombo <- unconstrained.BF(BFcombo, n)
    PMP <- PMP.update(BFcombo)
    PMP
}
