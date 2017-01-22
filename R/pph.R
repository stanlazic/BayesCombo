#' @title Calculates the posterior probability of hypotheses for one study
#'
#' @description The function takes a single effect size and its standard error
#' and calculates the posterior probability of each hypothesis (H<: the effect
#' size is less than 0, H0: the effect size is zero, or H>: the effect size is
#' greater than zero).
#'
#' @details Effect sizes could be a mean difference between groups, regression
#' slope, odds ratio, or other values provided by statistical models. The
#' standard errors represent the uncertainty in the effect size and are provided
#' by most statistical software.
#'
#' Two types of priors need to be specified. The first is the prior for the
#' effect size, which is given by a mean (usually zero) and standard error,
#' which can be specified by the user or calculated automatically. Second, prior
#' probabilities for each hypothesis need to be provided, and an equal
#' probability of 1/3 is used by default.
#'
#' The null hypothesis (usually effect size = 0) can be specified as either a
#' point null or as a range. A point null tests if the effect size is
#' \emph{exactly} zero while the range null tests if the effect size close
#' enough to zero for practical purposes. 'Close enough' is defined by the user
#' as a range on either size of zero.
#'
#' To combine multiple effect sizes use the \code{ev.combo} function.
#'
#' @param beta Effect size.
#' 
#' @param se.beta Standard error for the effect.
#' 
#' @param beta0 A prior value for the effect size. Default is zero.
#'
#' @param se0 A prior standard error for the effect size. Default is \code{NULL}
#' and is calculated automatically.
#' 
#' @param ci Is used to calculate the prior standard error if \code{se0 =
#' NULL}. The default value of 99 calculates the prior standard error so that
#' the 99% confidence intervals of the prior distribution are aligned with the
#' largest (furthest from zero) confidence interval of the data distribution.
#' 
#' @param se.mult Standard error multiplier used to increase or decrease the
#' prior SE and used in conjunction with \code{ci} when \code{se0 = NULL}.
#' 
#' 
#' @param H0 A vector of length two that defines the null hypothesis. If the
#' values are identical (e.g. \code{H0 = c(0,0)}) a point null is used,
#' otherwise the null is defined as the range between the lower and upper value.
#' 
#' @param H.priors Prior hypothesis probabilities; default is an equal
#' probability of 1/3, and they are specified in the following order:
#' \code{H<0}, \code{H=0}, \code{H>0}.
#' 
#' @param scale Logical. Whether to scale the effect size by its standard
#' error. Standardising has no effect on the calculations but standardised
#' effect sizes may be easier to compare in a forest plot.
#' 
#' @param adjust Logical. Whether to adjust very small posterior hypothesis
#' probabilities. Adjusting prevents a single study from having too much
#' influence on the results when combining multiple studies. For example, if the
#' probability for a hypothesis from one study is zero, then additional studies
#' cannot alter this probability (multiplying anything by zero is still zero).
#' 
#' @param epsilon A small value that a posterior hypothesis probability must
#' fall below before an adjustment is made. Ignored if \code{adjust = FALSE}.
#' 
#' @param adj.factor A small number added to each posterior hypothesis
#' probability if \code{adjust = TRUE} and one of the posterior hypothesis
#' probabilities is less than \code{epsilon}. The PPHs are then re-scaled to sum
#' to one.
#'
#' @return Object of class \code{pph} which contains the posterior hypothesis
#' probabilities and other calculated values.
#' 
#' @seealso \code{\link{plot.PPH}}, \code{\link{ev.combo}}
#' @export
#' @examples
#' # library(labstats) # need to install from CRAN
#' # plot(time.immob ~ dose, data=fluoxetine) 
#' # summary(lm(time.immob ~ dose, data=fluoxetine))
#' x <- pph(beta=-0.25200, se.beta=0.09913) # dose effect from above output
#' x

pph <- function(beta, se.beta, beta0 = 0, se0 = NULL, ci = 99,
                se.mult = 1, H0 = c(0,0), H.priors = rep(1/3, 3),
                scale = FALSE, adjust = FALSE, epsilon = 1e-6,
                adj.factor = 0.0001) {

    if (beta0 < H0[1] | beta0 > H0[2] ) {
        stop("beta0 must lie between the lower and upper bounds of H0.")
    }

    if (length(H0) != 2) {
        stop("H0 must have two values. Use c(0,0) for a point null at zero.")
    }

    if (any(se.beta <= 0)){
        stop("se.beta must be greater than zero.")
    }
    
    if (scale) { # standardise
        beta <- beta/se.beta
        se.beta <- se.beta/se.beta
    }

    if (ci <= 0 | ci >= 100 ) {
        stop("ci must lie between zero and one.")
    }

    if (is.null(se0)){ # calculate if not given
        se0 <- prior.se(beta, se.beta, ci) * se.mult
    }
    
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

    # PPHs
    pphs <- (bfs * H.priors) / sum(bfs * H.priors)
    names(pphs) <- c("H<", "H0", "H>")

    if (adjust & any(pphs < epsilon)){
        adj <- pphs + adj.factor
        pphs <- adj/sum(adj)
    }

    return(
        structure(list(beta = beta, se.beta = se.beta, beta0 = beta0,
                       ci = ci, se0 = se0, post.b = post.b,
                       post.se = post.se, H.priors = H.priors, BFs = bfs,
                       pphs = pphs),
                  class = "PPH")
        )
}
