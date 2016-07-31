#' @title Calculate the prior variances for the unconstrained model
#' 
#' @description Given a set of values for the effect sizes and their standard
#' errors a set of unconstrained prior variances are calculated. Not called
#' directly by the user.
#'
#' @details The prior variances are calculated by getting the largest confidence
#' interval from the prior mean by using the confidence interval of the observed
#' values.
#'
#' @param beta Vector of observed effect sizes.
#' @param se.beta Vector of standard errors for the effect sizes.
#' @param pi0 Vector of prior model probabilities.
#' @param beta0 Vector or value of prior means (usually 0).
#' @param percent Confidence interval percentage.
#'
#' @return vector of equal length to that of the observed variances.

prior.var <- function(beta, se.beta, beta0, pi0, percent = 99) {
    if (is.numeric(beta) != TRUE) {
        stop("beta is not numeric")
    }
    if (is.numeric(se.beta) != TRUE) {
        stop("se.beta is not numeric")
    }
    if (is.numeric(beta0) != TRUE) {
        stop("beta0 is not numeric")
    }
    if (is.numeric(pi0) != TRUE) {
        stop("pi0 is not numeric")
    } else if (sum(pi0) != 1) {
        stop("Prior model probabilities do not sum to 1")
    }
    
    BFcombo <- structure(list(beta = beta, se.beta = se.beta,
                              beta0 = beta0, pi0 = pi0), class = "BFcombo")
    
    # setup
    percent <- (percent / 100)
    percent <- percent + (1 - percent) / 2
    multiplier <- qnorm(percent)
    data <- matrix(0, ncol = 2, nrow = length(BFcombo$beta))
    
    # get min/max
    data[, 1] <- BFcombo$beta - BFcombo$se.beta * multiplier
    data[, 2] <- BFcombo$beta + BFcombo$se.beta * multiplier
    bounds <- abs(data)
    pVar <- apply(X = bounds, MARGIN = 1, max)
    pVar <- ( (pVar / multiplier)) ^ 2
    
    # update.object
    BFcombo$var0 <- pVar
    return(BFcombo)
}
