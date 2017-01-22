#' @title Calculates the posterior probability of hypotheses for multiple
#' studies
#'
#' @description The function takes multiple effect sizes and a their standard
#' errors and calculates the posterior probability for each hypothesis (H<: the
#' effect size is less than 0, H0: the effect size is zero, or H>: the effect
#' size is greater than zero).
#'
#' @details This function calls \code{pph()} once for each study to be combined,
#' where the posterior probabilities for one study are used as the priors for
#' the next study. One exeption is that values for \code{se0} are ignored as
#' they are calculated automatically.
#'
#' @inheritParams pph
#' 
#' @param ... Options to be passed to \code{pph()}.
#'
#' @return Object of class \code{EV} which contains a matrix of posterior
#' probabilities for each updated step and other calculated values.
#'
#' @export
#' @examples
#' x <- ev.combo(beta = c(0.0126, 5.0052, 1.2976, 0.0005),
#'        se.beta = c(0.050, 2.581, 2.054, 0.003) )
#' x
#' plot(x)
ev.combo <- function(beta, se.beta, beta0 = 0, ci = 99, H0 = c(0, 0),
                       scale = FALSE, H.priors = rep(1/3, 3), se.mult = 1,
                       adjust = FALSE, epsilon = 1e-6, adj.factor = 0.0001,
                       ... ) {

    if(length(beta) != length(se.beta)) {
           stop("beta and se.beta must be the same length.")
       }

    if(length(H.priors) != 3 || sum(H.priors) != 1 ) {
        stop("H.priors must have 3 values that sum to one.")
    }

    n.studies <- length(beta)

    se0 <- numeric(n.studies)
    post.b <- numeric(n.studies)
    post.se <- numeric(n.studies)

    # pphs with uniform priors
    res.uni <- matrix(NA, nrow = n.studies, ncol = 3)
    colnames(res.uni) <- c("H<", "H0", "H>")
    
    # running total pphs

    res <- matrix(NA, nrow = n.studies + 1, ncol = 3)
    res[1,] <- H.priors
    colnames(res) <- c("H<", "H0", "H>")


    for (i in 1:n.studies) {

        # uniform priors
        tmp.uni <- pph(beta = beta[i], se.beta = se.beta[i], beta0 = beta0,
                       ci = ci, H0 = H0, H.priors = rep(1/3, 3),
                       se.mult = se.mult, adjust = adjust, epsilon = epsilon,
                       adj.factor = adj.factor, scale=scale)

        # running total
        tmp <- pph(beta = beta[i], se.beta = se.beta[i], beta0 = beta0,
                   ci = ci, H0 = H0, H.priors = res[i,],
                   se.mult = se.mult, adjust = adjust, epsilon = epsilon,
                   adj.factor = adj.factor, scale=scale)

        se0[i] <- tmp$se0
        post.b[i] <- tmp$post.b
        post.se[i] <- tmp$post.se
        res.uni[i, ] <- tmp.uni$pphs
        res[i+1,] <- tmp$pphs

    }

    if (scale) { # standardise
        beta <- beta/se.beta
        se.beta <- se.beta/se.beta
    }
    
    return(
        structure(list(N=n.studies, beta = beta, se.beta = se.beta,
                       beta0 = beta0, ci = ci, se0 = se0,
                       post.b = post.b, post.se = post.se, H.priors = H.priors,
                       pph.uniform = res.uni, pphs = res),
                  class = "EV")
        )
}
