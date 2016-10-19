#' @title Calculates posterior model probabilities for multiple studies
#'
#' @description The function takes multiple effect size and a their standard
#' errors and calculates the posterior model probabilities for each hypothesis
#' (H<: the effect size is less than 0, H0: the effect size is zero, or H>: the
#' effect size is greater than zero).
#'
#' @details This function calls \code{\link{pmp}} once for each study to be
#' combined where the posterior model probabilities for one study are used as
#' the priors for the next study. The inputs are identical to the
#' \code{\link{pmp}} function, so please see that documentation for details.
#'
#' @inheritParams pmp
#' 
#' @param ... Options to be passed to \code{\link{pmp}}.
#'
#' @return Object of class \code{PMPlist} which contains a matrix of posterior
#' model probabilities for each updated step and other calculate values
#'
#' @export
#' @examples
#' x <- pmp.update(beta = c(0.0126, 5.0052, 1.2976, 0.0005),
#'        se.beta = c(0.050, 2.581, 2.054, 0.003) )
#' x
#' plot(x)
pmp.update <- function(beta, se.beta, beta0=0, percent=99, H0=c(0,0),
                       scale=FALSE, mod.priors=rep(1/3, 3), var.mult = 1,
                       adjust=FALSE, epsilon=1e-6, adj.factor=0.0001,
                       ... ) {

    if(length(beta) != length(se.beta)) {
           stop("beta and se.beta must be the same length.")
       }

    if(length(mod.priors) != 3 || sum(mod.priors) != 1 ) {
        stop("mod.priors must have 3 values that sum to one.")
    }

    n.studies <- length(beta)

    if (scale) { # standardise
        beta <- beta/se.beta
        se.beta <- se.beta/se.beta
    }


    se0 <- numeric(n.studies)
    post.b <- numeric(n.studies)
    post.se <- numeric(n.studies)

    # pmps with uniform priors
    res.uni <- matrix(NA, nrow = n.studies, ncol = 3)
    colnames(res.uni) <- c("H<","H0","H>")
    
    # running total pmps

    res <- matrix(NA, nrow = n.studies + 1, ncol = 3)
    res[1,] <- mod.priors
    colnames(res) <- c("H<","H0","H>")


    for (i in 1:n.studies) {

        # uniform priors
        tmp.uni <- pmp(beta = beta[i], se.beta = se.beta[i], beta0 = beta0,
                       percent = percent, H0 = H0, mod.priors = rep(1/3, 3),
                       var.mult = var.mult, adjust = adjust, epsilon = epsilon,
                       adj.factor = adj.factor)

        # running total
        tmp <- pmp(beta = beta[i], se.beta = se.beta[i], beta0 = beta0,
                   percent = percent, H0 = H0, mod.priors = res[i,],
                   var.mult = var.mult, adjust = adjust, epsilon = epsilon,
                   adj.factor = adj.factor)

        se0[i] <- tmp$se0
        post.b[i] <- tmp$post.b
        post.se[i] <- tmp$post.se
        res.uni[i, ] <- tmp.uni$pmps
        res[i+1,] <- tmp$pmps

    }

    return(
        structure(list(N=n.studies, beta = beta, se.beta = se.beta,
                       beta0 = beta0, percent = percent, se0 = se0,
                       post.b = post.b, post.se = post.se, mod.priors=mod.priors,
                       pmp.uniform = res.uni, pmps = res),
                  class = "PMPlist")
        )
}
