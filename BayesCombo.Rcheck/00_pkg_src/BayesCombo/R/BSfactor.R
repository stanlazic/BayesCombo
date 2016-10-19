#' @title Calculate a 'Bayesian Safety' (BS) factor
#'
#' @description Calculates the prior probability for the null that gives a just
#' significant result.
#'
#' @details When the posterior probability for a non-null hypothesis is large,
#' it may be of interest to calculate the prior for the null that gives a just
#' significant result. Here 'significant' is a threshold that the posterior must
#' exceed. For example, if a series of experiments on homeopathy gives a
#' posterior probability of 0.98 in favour homeopathy's effectiveness, we can
#' calculate the strength of the prior needed to make the posterior just pass
#' the 0.98 threshold. If the BS factor is very large (sceptical prior), then we
#' may be more inclined to believe the results because they were sufficient to
#' shift a strong prior belief in no effect. If the data can only overcome a
#' weak prior, then the results may be unconvincing, despite having a large
#' posterior probability. We can ask ourselves if our prior is greater or less
#' than the BS factor, and judge the results accordingly.
#'
#' @param x An object of class \code{PMPlist}.
#' 
#' @param sig The 'significance' cut-off value for a non-null posterior
#' model probability.
#' 
#' @param n Number of steps between 0.33333 and 0.99999 to calculate the BS
#' factor. Increasing n gives a more accurate estimate.
#'
#' @return Object of class 'BSfactor' which contains a matrix of final posterior
#' model probabilities, a matrix of prior model probabilities, the 'boundary'
#' and which hypothesis is being considered.
#'
#' @seealso \code{\link{pmp.update}}
#'
#' @export
#' @examples
#'
#' x <- pmp.update(beta = c(0.126, 5.005, 1.298, 0.000476),
#'        se.beta = c(0.0504, 2.5811, 2.0541, 0.0026) )
#'
#' BSfactor(x)

BSfactor <- function(x, n = 100, sig = 0.95) {

    if (!inherits(x, "PMPlist")) {
        stop("Input must be a 'PMPlist' object from the pmp.update function.")
    }
        

    if (which.max(x$pmps[x$N, ]) == 1 ) {
        stop("H0 already has the highest support")
    }

    
    # find biggest PMP
    max.ind <- which.max(x$pmps[x$N, ])
    max.pmp <- x$pmps[x$N, max.ind] 
    if (max.pmp < sig){
        stop("Largest non-null PMP needs to be greather than sig")
    }
       
    values <- seq(0.33333, 0.99999, length.out = n)


    # matrix of priors
    priors <- cbind("H<" = (1 - values) / 2, "H0" = values,
                    "H>" = (1 - values) / 2 )
    
    output <- matrix(NA, nrow=n, ncol=3)
    colnames(output) <- colnames(priors)
    
    for (i in 1:n) {
        
        temp <- pmp.update(x$beta, x$se.beta, x$beta0,
                              mod.priors = priors[i, ])
        output[i, ] <- temp$pmps[x$N, ]
    }

    # just sig diff
    jsd.ind <- which(output[, max.ind] <= sig)[1] - 1

    if (length(jsd.ind) == 0) {
        jsd.ind <- n
        warning("Prior is at upper boundary")
    }

    ### need to check if at boundary
    
    return(structure(list(PMP = output, priorMP = priors,
                          boundary = priors[jsd.ind, ],
                          sig = sig),
                     class = "BSfactor"))
}
