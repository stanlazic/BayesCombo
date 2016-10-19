#' @title Print and summary for PMP, PMPlist, and BSfactor objects
#'
#' @description Print and summary methods for PMP, PMPlist, BSfactor objects.
#'
#' @param x A \code{PMP}, \code{PMPlist}, or \code{BSfactor} object.
#' @param digits Number of digits to show.
#'
#' @return A list of posterior model probabilities and calculated values.
#' @method summary PMP

# summarise one study
summary.PMP <- function(x, digits=4){
    return(round(x$pmps, digits))
}


#' @rdname summary.PMP
#' @method print PMP

print.PMP <- function(x, digits=4){
    return(list("post.b"=round(x$post.b, digits),
                "post.se"=round(x$post.se, digits),
                "BFs"=round(x$BFs, digits),
                "pmps"=round(x$pmps, digits))) 
}


# summarise multiple studies
#' @rdname summary.PMP
#' @method summary PMPlist

summary.PMPlist <- function(x, digits=4){
    return(round(x$pmps, digits))
}


#' @rdname summary.PMP
#' @method print PMPlist

print.PMPlist <- function(x, digits=4){
    return(list("post.b"=round(x$post.b, digits),
                "post.se"=round(x$post.se, digits),
                "pmps"=round(x$pmps, digits))) 
}


# summarise multiple studies
#' @rdname summary.PMP
#' @method summary BSfactor

summary.BSfactor <- function(x, digits=4){
    return(round(x$boundary, digits))
}
