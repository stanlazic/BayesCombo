#' @title Print and summary methods for PPH and EV objects
#'
#' @description Print and summary methods for PPH and EV objects.
#'
#' @param x A \code{PPH} or \code{EV} object.
#' @param digits Number of digits to show.
#'
#' @return A list of posterior hypothesis probabilities and other calculated
#' values.
#' @method summary PPH

# summarise one study
summary.PPH <- function(x, digits=4){
    return(round(x$pphs, digits))
}


#' @rdname summary.PPH
#' @method print PPH

print.PPH <- function(x, digits=4){
    return(list("post.b"=round(x$post.b, digits),
                "post.se"=round(x$post.se, digits),
                "BFs"=round(x$BFs, digits),
                "pphs"=round(x$pphs, digits))) 
}


# summarise multiple studies
#' @rdname summary.PPH
#' @method summary EV

summary.EV <- function(x, digits=4){
    return(round(x$pphs, digits))
}


#' @rdname summary.PPH
#' @method print EV

print.EV <- function(x, digits=4){
    return(list("post.b"=round(x$post.b, digits),
                "post.se"=round(x$post.se, digits),
                "pphs"=round(x$pphs, digits))) 
}
