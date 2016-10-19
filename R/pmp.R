#' @title Calculates posterior model probabilities for one study
#'
#' @description The function takes a single effect size and a its standard error
#' and calculates the posterior model probabilities for each hypothesis (H<: the
#' effect size is less than 0, H0: the effect size is zero, or H>: the effect
#' size is greater than zero).
#'
#' @details Effect sizes are typically a mean difference between groups, a
#' regression slope, odds ratio, and so on, that are provided by generalised
#' linear models. The standard errors for the effect size are provided in the
#' output from functions that calculate the effect size. Two types of priors
#' need to be specified. The first is the prior for the effect size, which is
#' given by a mean (usually zero) and variance, which is calculated
#' automatically. Second, prior probabilities for each hypothesis need to be
#' provided, and an equal probability of 1/3 is used by default.
#'
#' The null hypothesis (usually effect size = 0) can be specified as either a
#' point null or as a range. A point null tests if effect size is \emph{exactly}
#' zero while the range null tests if the effect size close enough to zero for
#' practical purposes. "Close enough" is defined by the user as a range on
#' either size of zero.
#'
#' To combine multiple effect sizes use the \code{pmp.update} function.
#'
#' @param beta An numeric effect size.
#' 
#' @param se.beta A numeric standard error of the effect.
#' 
#' @param beta0 A numeric prior value for the effect size. Default is zero.
#' 
#' @param percent A numeric value that is used to calculate the prior
#' variance. The default value of 99 calculates the prior variance so that the
#' 99% confidence intervals of the prior are aligned with the largest (furthest
#' from zero) confidence intervals of the data distribution.
#' 
#' @param var.mult Variance multiplier used to increase or decrease the prior
#' variance.
#' 
#' @param H0 A vector of length two that defines the null hypothesis. If the
#' values are identical (e.g. \code{H0 = c(0,0)}) a point null is used,
#' otherwise the null is defined as the range between the lower and upper value.
#' 
#' @param mod.priors Prior model probabilities; default is an equal probability
#' of 1/3.
#' 
#' @param scale Logical. Whether to scale the effect size by its standard
#' error. This has no effect on the calculations but it may be useful to use
#' standardised effect sizes.
#' 
#' @param adjust Logical, whether to adjust the posterior model probabilities if
#' they are too small. When combining multiple studies, this prevents a single
#' study from having too much influence on the results. For example, if the
#' probability for a hypothesis is zero, then additional studies cannot alter
#' this probability.
#' 
#' @param epsilon A small value that a posterior model probability must be below
#' before an adjustment is made.
#' 
#' @param adj.factor A small number added to each posterior model probability if
#' \code{adjust=TRUE} and a PMP is less than \code{epsilon}. The PMPs are then
#' re-scaled to sum to one.
#'
#' @return Object of class \code{pmp} which contains the posterior model
#' probabilities and other calculate values.
#' 
#' @seealso \code{\link{plot.PMP}}, \code{\link{pmp.update}}
#' @export
#' @examples
#' # library(labstats) # need to install separately
#' # plot(time.immob ~ dose, data=fluoxetine) 
#' # summary(lm(time.immob ~ dose, data=fluoxetine))
#' x <- pmp(beta=-0.25200, se.beta=0.09913) # dose effect from above output
#' x

pmp <- function(beta, se.beta, beta0 = 0, percent = 99, var.mult = 1,
                H0 = c(0,0), mod.priors = rep(1/3, 3), scale=FALSE,
                adjust=FALSE, epsilon=1e-6, adj.factor=0.0001) {

    if (beta0 < H0[1] | beta0 > H0[2] ) {
        stop("beta0 must lie between the lower and upper bounds of H0.")
    }

    if (length(H0) != 2) {
        stop("H0 must have two values. Use c(0,0) for a point null at zero.")
    }

    if (scale) { # standardise
        beta <- beta/se.beta
        se.beta <- se.beta/se.beta
    }

    se0 <- prior.se(beta, se.beta, percent) * var.mult
    post.b <- calc.post.beta(beta, se.beta, beta0, se0)
    post.se <- calc.post.se(se.beta, se0)


    # BFs
    lt <- pnorm(H0[1], post.b, post.se) / pnorm(H0[1], beta0, se.beta)

    gt <- pnorm(H0[2], post.b, post.se, lower.tail = FALSE) /
          pnorm(H0[2], beta0, se.beta, lower.tail = FALSE)

    if (H0[1] == H0[2]){ # point null

        null <- dnorm(beta0, post.b, post.se) / dnorm(H0[1], beta0, se0)

    } else { # range null

        null <- (pnorm(H0[2], post.b, post.se) - pnorm(H0[1], post.b, post.se)) /
            (pnorm(H0[2], beta0, se.beta) - pnorm(H0[1], beta0, se.beta))
    }

    bfs <-c("H<" = lt, "H0" = null, "H>" = gt)

    # PMPs
    pmps <- (bfs * mod.priors)/ sum(bfs * mod.priors)
    names(pmps) <- c("H<","H0","H>")

    if (adjust & any(pmps < epsilon)){
        adj <- pmps + adj.factor
        pmps <- adj/sum(adj)
    }

    return(
        structure(list(beta = beta, se.beta = se.beta, beta0 = beta0,
                       percent = percent, se0 = se0, post.b = post.b,
                       post.se = post.se, mod.priors=mod.priors, BFs = bfs,
                       pmps = pmps),
                  class = "PMP")
        )
}
