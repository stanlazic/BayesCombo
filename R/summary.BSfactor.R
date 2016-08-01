#' @title Summarise a BSfactor object
#'
#' @description Gives summary of various sections of the BSfactor class.
#'
#' @details For all section of the BSfactor object which are matrices, the
#' summary function will return the first and last rows with the values rounded
#' to 4 decimal points.  The summary also contains other key pieces of
#' information such as boundary and threshold.
#'
#' @param object A object of the class BSfactor
#' @param ... Additional parameters not required
#'
#' @return A summary of the BSfactor object.
#'
#' @seealso \code{ \link{BSfactor}}
#'
#' @export
#' @examples
#' x <- BSfactor( beta = c(0.0126, 5.0052, 1.2976, 0.0005),
#'        se.beta = c(0.050, 2.581, 2.054, 0.003),
#'        beta0 = 0, reverse = TRUE )
#' summary(x)


summary.BSfactor <- function(object, ...) {
    if (class(object) != "BSfactor") {
        warning("please input object of class BSfactor")
        stop()
    }
    ### PMP
    n <- length(object$PMP[, 1])
    PMP <- priorMP <- matrix(0, nrow = 2, ncol = 3)
    PMP[1, ] <- round(object$PMP[1, ], 4)
    PMP[2, ] <- round(object$PMP[n, ], 4)
    colnames(PMP) <- colnames(object$PMP)

    ### priorMP
    priorMP[1, ] <- round(object$priorMP[1, ], 4)
    priorMP[2, ] <- round(object$priorMP[n, ], 4)
    colnames(priorMP) <- colnames(object$PMP)

    ### boundary
    if(is.null(object$boundary)){
      boundary <- object$boundary <- NULL
    } else{
      boundary <- round(object$boundary, 4)
    }

    ### threshold
    threshold <- round(object$threshold, 4)

    ### hypothesis

    hypothesis <- object$hypothesis

    return(list(PMP = PMP,
                priorMP = priorMP,
                boundary = boundary,
                threshold = threshold,
                hypothesis = hypothesis))
}
