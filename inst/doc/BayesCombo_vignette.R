## ---- echo = FALSE, include=FALSE----------------------------------------
library(knitr)
opts_chunk$set(fig.align='center') 

## ----setup, echo = FALSE, warning= FALSE, message=FALSE------------------
library(BayesCombo)

## ---- fig.height=4, fig.width=4------------------------------------------
library(labstats)
lattice::xyplot(time.immob ~ dose, data=fluoxetine, type=c("g","p","r"))

## ------------------------------------------------------------------------
summary(lm(time.immob ~ dose, data=fluoxetine))$coef

x <- pmp(beta = -0.252, se.beta = 0.099, percent=95)
summary(x)

## ---- fig.height=5, fig.width=5------------------------------------------
par(las=1)
plot(x, leg.loc = "topright")

## ---- fig.height=5, fig.width=5------------------------------------------
x2 <- pmp(beta = -0.252, se.beta = 0.099, var.mult = 2)
summary(x2)

par(las=1)
plot(x2, leg.loc = "topright")

## ------------------------------------------------------------------------
x <- pmp.combo(beta = c(2.3, 1.2, 0.2, 0.44),
	              se.beta = c(1.03, 0.75, 0.16, 0.28))

## ----fig.height=5, fig.width=5-------------------------------------------
par(las=1)
forestplot(x)
abline(v=0, lty=2)

## ----summaryPMP----------------------------------------------------------
summary(x)

## ----fig.height=4, fig.width=6-------------------------------------------
par(las=1)
plot(x)

## ----fig.height=5, fig.width=8-------------------------------------------
par(mfrow=c(1,2))
dotchart(x$pmp.uniform, xlim=c(0,1), xlab="PMP")
dotchart(t(x$pmp.uniform), xlim=c(0,1), xlab="PMP")

