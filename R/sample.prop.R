#' @title Calculates sampling proportions for the unconstrained parameters
#'
<<<<<<< HEAD
#' @description Done using sampling with the prior and posterior means and
#' variances.
=======
#' @description Sampling the prior and posterior distributions given their means and variances
>>>>>>> bc3ff0fc739b0e89bbf38afc008be33d6faede7d
#'
#' @details This function is called within the unconstrained.BF function and is
#' not called directly by the user.
#'
#' @param n Number of times the distributions are sampled.
#' @param prior.mean Vector of unconstrained prior model means. This is part of
#' the BFcombo object.
#' @param prior.sd Vector of unconstrained prior model standard deviations. The
#' variances are part of the BFcombo object.
#' @param posterior.mean Vector of unconstrained posterior model means. This is
#' part of the BFcombo object.
#' @param posterior.sd Vector of unconstrained posterior model standard
#' devidations. The variances are part of the BFcombo object.
#' @param hypothesis Choose the hypothesis to sample from.
#'
#' @seealso \code{\link{unconstrained.BF}}

sample.prop <- function(n, prior.mean, posterior.mean,
                        prior.sd, posterior.sd, hypothesis = 1) {
    if (hypothesis == "1") {
        "%a%" <- function(x, y) {
            x > y
        }
    } else {
        "%a%" <- function(x, y) {
            x < y
        }
    }
    n.post <- length(posterior.mean)
    Cm <- Fm <- names.vec <- rep(0, n.post)
    Cm[] <- 0.5
    for (i in 1:n.post) {
        Fm[i] <- mean(ifelse(rnorm(n, posterior.mean[i],
                                   posterior.sd[i]) %a% prior.mean, 1, 0))
        names.vec[i] <- paste("prob.post", i, sep = "")
    }
    out <- matrix(0, ncol = (n.post), nrow = 2)
    rownames(out) <- c("Fm", "Cm")
    colnames(out) <- names.vec
    out[2, ] <- Cm
    out[1, ] <- Fm
    out
}
