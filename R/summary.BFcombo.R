#' @title Summarise a BFcombo object
#'
#' @description Gives summary of various sections of the BFcombo class.
#'
#' @details For all the sections which provide a mean or variance vector, the
#' summary function will return a minimum and maximum value.  For a Bayes factor
#' or probability, the summary function will return a matrix with the values
#' rounded to 4 decimal points.
#'
#' @param object A object of the class BFcombo
#' @param ... Additional parameters
#'
#' @return A summary of the BFcombo object passed.
#'
#' @seealso \code{ \link{calculate.PMP} \link{prior.var}}
#'
#' @export
#' @examples
#' x <- calculate.PMP( beta = c(0.0126, 5.0052, 1.2976, 0.0005),
#'        se.beta = c(0.050, 2.581, 2.054, 0.003) )
#' summary(x)


summary.BFcombo <- function(object, ...) {
    # get categories
    categ <- names(object)

    if (all(categ != "se.beta")) {
        output <- list(Observed = NULL)
        warning("Only object inputs included")

        Min_Obs <- round(c(min(object$beta), min(object$se.beta^2)), digits = 4)
        Max_Obs <- round(c(max(object$beta), max(object$se.beta^2)), digits = 4)

        output$Observed <- data.frame(Min = Min_Obs, Max = Max_Obs)
        rownames(output$Observed) <- c("Mean", "Variance")

    } else if (all(categ != "unBetaPost")) {

        output <- list(Observed = NULL, uPrior = NULL)

        Min_Obs <- round(c(min(object$beta), min(object$se.beta^2)), digits = 4)
        Max_Obs <- round(c(max(object$beta), max(object$se.beta^2)), digits = 4)

        output$Observed <- data.frame(Min = Min_Obs, Max = Max_Obs)
        rownames(output$Observed) <- c("Mean", "Variance")

        ###


        Min_uPrior <- round(c(min(object$beta0),
                              min(object$var0)), digits = 4)
        Max_uVariance <- round(c(max(object$beta0),
                                 max(object$var0)), digits = 4)

        output$Observed <- data.frame(Min = Min_Obs, Max = Max_Obs)
        rownames(output$Observed) <- c("Mean", "Variance")

        output$uPrior <- data.frame(Min = Min_uPrior, Max = Max_uVariance)
        rownames(output$uPrior) <- c("Mean", "Variance")

    } else if (all(categ != "BFmu")) {
        output <- list(Observed = NULL, uPrior = NULL, uPosterior = NULL)

        Min_Obs <- round(c(min(object$beta),
                           min(object$se.beta^2)), digits = 4)
        Max_Obs <- round(c(max(object$beta),
                           max(object$se.beta^2)), digits = 4)
        Min_uPrior <- round(c(min(object$beta0),
                              min(object$var0)), digits = 4)
        Max_uVariance <- round(c(max(object$beta0),
                                 max(object$var0)), digits = 4)
        Min_uPosterior <- round(c(min(object$unBetaPost),
                                  min(object$unVarPost)), digits = 4)
        Max_uPostVariance <- round(c(max(object$unBetaPost),
                                     max(object$unVarPost)), digits = 4)
        output$Observed <- data.frame(Min = Min_Obs, Max = Max_Obs)
        rownames(output$Observed) <- c("Mean", "Variance")
        output$uPrior <- data.frame(Min = Min_uPrior, Max = Max_uVariance)
        rownames(output$uPrior) <- c("Mean", "Variance")
        output$uPosterior <- data.frame(Min = Min_uPosterior,
                                        Max = Max_uPostVariance)
        rownames(output$uPosterior) <- c("Mean", "Variance")

    } else if (all(categ != "PMP")) {
        output <- list(Observed = NULL, uPrior = NULL, uPosterior = NULL,
                       BFmu = NULL)
        Min_Obs <- round(c(min(object$beta), min(object$se.beta^2)),
                         digits = 4)
        Max_Obs <- round(c(max(object$beta), max(object$se.beta^2)),
                         digits = 4)
        Min_uPrior <- round(c(min(object$beta0), min(object$var0)),
                            digits = 4)
        Max_uVariance <- round(c(max(object$beta0), max(object$var0)),
                               digits = 4)
        Min_uPosterior <- round(c(min(object$unBetaPost),
                                  min(object$unVarPost)), digits = 4)
        Max_uPostVariance <- round(c(max(object$unBetaPost),
                                     max(object$unVarPost)), digits = 4)
        output$BFmu <- round(object$BFmu, digits = 4)
        output$Observed <- data.frame(Min = Min_Obs, Max = Max_Obs)
        rownames(output$Observed) <- c("Mean", "Variance")
        output$uPrior <- data.frame(Min = Min_uPrior,
                                    Max = Max_uVariance)
        rownames(output$uPrior) <- c("Mean", "Variance")
        output$uPosterior <- data.frame(Min = Min_uPosterior,
                                        Max = Max_uPostVariance)
        rownames(output$uPosterior) <- c("Mean", "Variance")

    } else if (any(categ == "PMP")) {
        output <- list(Observed = NULL, uPrior = NULL, uPosterior = NULL,
                       BFmu = NULL, PMP = NULL)
        Min_Obs <- round(c(min(object$beta),
                           min(object$se.beta^2)), digits = 4)
        Max_Obs <- round(c(max(object$beta),
                           max(object$se.beta^2)), digits = 4)
        Min_uPrior <- round(c(min(object$beta0),
                              min(object$var0)), digits = 4)
        Max_uVariance <- round(c(max(object$beta0),
                                 max(object$var0)), digits = 4)
        Min_uPosterior <- round(c(min(object$unBetaPost),
                                  min(object$unVarPost)), digits = 4)
        Max_uPostVariance <- round(c(max(object$unBetaPost),
                                     max(object$unVarPost)), digits = 4)
        output$BFmu <- round(object$BFmu, digits = 4)
        output$PMP <- round(object$PMP, digits = 4)
        output$Observed <- data.frame(Min = Min_Obs,
                                      Max = Max_Obs)
        rownames(output$Observed) <- c("Mean", "Variance")
        output$uPrior <- data.frame(Min = Min_uPrior,
                                    Max = Max_uVariance)
        rownames(output$uPrior) <- c("Mean", "Variance")
        output$uPosterior <- data.frame(Min = Min_uPosterior,
                                        Max = Max_uPostVariance)
        rownames(output$uPosterior) <- c("Mean", "Variance")

    } else {
        stop("Please use an input of class BFcombo")
    }
    output
}
