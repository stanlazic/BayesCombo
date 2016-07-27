#' @title Calculate unconstrained Posterior Model parameters.
#'
#' @description Calculates the unconstrained posterior model parameters which will then be used to calculate
#' unconstained Bayes factors.
#'
#' @details See vignette for detailed explanation.
#'
#' @param BFcombo A object of the class BFcombo but must also incldes a section with unconstrained prior variances
#'
#' @return A set of unconstrained posterior variances and means.
#'
#' @seealso \code{\link{prior.var}}
#'

u.post.param <- function(BFcombo) {
    if (any(names(BFcombo) == "var0")) {
        betaVar <- 1 / ( (1 / BFcombo$var0) + (1 / BFcombo$var1))
        betaTilda <- (BFcombo$beta0 * (1 / BFcombo$var0) + BFcombo$beta1 *
                        (1 / BFcombo$var1)) * betaVar
        out <- rbind(betaTilda, betaVar)
        rownames(out) <- c("u.beta.post", "u.var.post")
        BFcombo$unBetaPost <- out[1, ]
        BFcombo$unVarPost <- out[2, ]
        return(BFcombo)
    } else {
        if (class(BFcombo) != "BFcombo") {
            stop("Input not of BFcombo class please run prior.var function")
        } else {
            stop("Unconstrained prior variances not found.
                 Please run prior.var function")
        }
    }

}
