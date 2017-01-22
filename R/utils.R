#' @title Utility functions
#'
#' @description Not called directly by user.
#'
#' @param beta  Effect size.
#' @param se.beta Standard error of the effect.
#' @param beta0  Prior for the effect size.
#' @param se0 Prior standard error.
#' @param ci A numeric value that is used to calculate the prior standard error.
#' 
# posterior mean (standard Bayesian updating for normal conjugate prior)
calc.post.beta <- function(beta, se.beta, beta0, se0){
    ((beta/se.beta^2) + (beta0/se0^2)) / ((1/se.beta^2) + (1/se0^2))
}


#' @rdname calc.post.beta
# Posterior variance
calc.post.se <- function(se.beta, se0){
    sqrt(1 / ((1/se.beta^2) + (1/se0^2)) )
}


#' @rdname calc.post.beta
# Prior variance
prior.se <- function(beta, se.beta, ci) {
    
    if (!is.numeric(beta))    stop("beta is not numeric")
    if (!is.numeric(se.beta)) stop("se.beta is not numeric")
    
    # multiplier for confidence intervals
    ci <- (ci / 100)
    ci <- ci + (1 - ci) / 2
    multiplier <- qnorm(ci)
    data <- matrix(NA, ncol = 2, nrow = length(beta))
    
    # get min/max
    data[, 1:2] <- beta + c(-se.beta, se.beta) * multiplier

    bounds <- abs(data)
    se0 <- apply(bounds, 1, max)
    se0 <- ( (se0 / multiplier))
    
    return(se0)
}

