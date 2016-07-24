#' @title Plot of Prior Variances given Observed Means and Variances
#'
#' @description Produces a visualisation of prior variances by plotting the confidence intervals around the
#'  prior mean and also shows the observed means and variances as confidence intervals.
#'
#'
#' @param x A object of the class BFcombo (see make_BFcombo)
#' @param ... additional parameters not required
#'
#' @return Plot of Prior and observed confidence intervals.
#' @seealso \code{\link{calculate.PMP}}
#' @import ggplot2
#' @import stats graphics
#'
#' @export
#' @examples
#' x <- calculate.PMP( beta1 = c(0.0126474408, 5.0051724138, 1.2975612498, 0.0004762455),
#'     var1 = c(2.538974e-03, 6.662216e+00, 4.219142e+00, 6.963380e-06),
#'     beta0 = 0, pi0 = rep(1/3,3) )
#' plot(x)

plot.BFcombo<-function(x, ... ){
  #Setup
  n.studies<- length(x$beta1)
  data<- matrix(0, ncol = 2 , nrow = n.studies)
  y<- seq(0,1,length.out = 2*n.studies)
  y1<-y[seq_along(y)%% 2 == 0]
  y2<-y[seq_along(y)%% 2 != 0]

  #begin calculation
  percent<- (x$percent / 100)
  percent<- percent + (1-percent)/2
  multiplier<- qnorm(percent)
  # get min/max
  data[,1]<- x$beta1 - sqrt(x$var1)*multiplier
  data[,2]<- x$beta1 + sqrt(x$var1)*multiplier
  bounds<- abs(data)
  output <- apply(X = bounds,MARGIN = 1,max)

  X3<- 0 - sqrt(x$var0)*multiplier
  X4 <-0 + sqrt(x$var0)*multiplier

  data<- data.frame(data,X3,X4, y1,y2,x$beta1)

  # ## ggplot
  X1 <- X2 <- NULL
  p<- ggplot(data, aes(X1,y1))
  p<- p + geom_point(aes(X1,y1), shape = 124,size = 4) + geom_point(aes(X2,y1),shape = 124,size = 4)
  p<- p + geom_vline(xintercept = 0,linetype = 2 )
  p<- p + geom_segment(aes(x =X1, y = y1 , xend = X2, yend = y1))
  p<- p + geom_point(aes(X3,y2),col = "red" ,shape = 124,size = 4) + geom_point(aes(X4,y2), col = "red",shape = 124,size = 4)
  p<- p + geom_segment(aes(x =X3, y = y2 , xend = X4, yend = y2),col = "red")
  p<- p + geom_point(aes(x$beta1,y1))
  p<- p + scale_y_continuous(breaks= c(y1,y2), labels =  as.character(rep(1:n.studies,times = 2)) )
  p<- p + xlab("Effect size") + ylab("Study")
  p<- p + ggtitle("Prior Variance Selection")
  p
}
