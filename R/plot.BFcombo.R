#' @title Plot of Prior Variances given Observed Means and Variances
#'
#' @description Produces a visualisation of prior variances by plotting the confidence intervals around the
#'  prior mean and also shows the observed means and variances as confidence intervals.
#'
#' @details
#'
#'
#' @param BFcombo A object of the class BFcombo (see make_BFcombo).
#' @param Percent Sets the confidence interval. Default is 99.
#'
#' @return Plot of Prior and observed confidence intervals.
#' @seealso \code{\link{make_BFcombo}}
#'
#' @examples
#' # High values are desirable
#' x <- make_BFcombo( beta1 = c(0.090,0.140,1.090,1.781), var1 = c(0.000841,0.002916,0.008649,0.032041), beta0 = 0, pi0 = rep(1/3,3) )
#' plot(x)

plot.BFcombo<-function(BFcombo,percent = 99){
  #Setup
  n.studies<- length(BFcombo$beta1)
  data<- matrix(0, ncol = 2 , nrow = n.studies)
  y<- seq(0,1,length.out = 2*n.studies)
  y1<-y[seq_along(y)%% 2 == 0]
  y2<-y[seq_along(y)%% 2 != 0]

  #begin calculation
  percent<- (percent / 100)
  percent<- percent + (1-percent)/2
  multiplier<- qnorm(percent)
  # get min/max
  data[,1]<- BFcombo$beta1 - sqrt(BFcombo$var1)*multiplier
  data[,2]<- BFcombo$beta1 + sqrt(BFcombo$var1)*multiplier
  bounds<- abs(data)
  output <- apply(X = bounds,MARGIN = 1,max)

  X3<- 0 - output
  X4 <-0 + output

  data<- data.frame(data,X3,X4, y1,y2,BFcombo$beta1)

  ## ggplot

  p<- ggplot(data, aes(X1,y1))
  p<- p + geom_point(aes(X1,y1), shape = 124,size = 4) + geom_point(aes(X2,y1),shape = 124,size = 4)
  p<- p + geom_vline(xintercept = 0,linetype = 2 )
  p<- p + geom_segment(aes(x =X1, y = y1 , xend = X2, yend = y1))
  p<- p + geom_point(aes(X3,y2),col = "red" ,shape = 124,size = 4) + geom_point(aes(X4,y2), col = "red",shape = 124,size = 4)
  p<- p + geom_segment(aes(x =X3, y = y2 , xend = X4, yend = y2),col = "red")
  p<- p + geom_point(aes(BFcombo$beta1,y1))
  p<- p + geom_text(aes(BFcombo$beta1,y1,label = paste("beta" , "^",1,1:n.studies,"*", "'' %+-% '' ","*", round(multiplier,3),"*","sigma","^",11:14, sep = "")),parse = TRUE,nudge_y = -0.03)
  p<- p + geom_text(aes(rep(0,4),y2,label = paste("beta" , "^",1:n.studies,"*", "'' %+-% '' ","*", round(multiplier,3),"*","sigma","^",1:4, sep = "")),parse = TRUE,nudge_y = -0.03,col = "red")
  p<- p + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
  p<- p + xlab(NULL) + ylab(NULL)
  p<- p + ggtitle("Prior Variance Selection")
  p
}
