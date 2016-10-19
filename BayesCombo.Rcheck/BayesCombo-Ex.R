pkgname <- "BayesCombo"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "BayesCombo-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('BayesCombo')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("BSfactor")
### * BSfactor

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: BSfactor
### Title: Calculate a 'Bayesian Safety' (BS) factor
### Aliases: BSfactor

### ** Examples


x <- pmp.update(beta = c(0.126, 5.005, 1.298, 0.000476),
       se.beta = c(0.0504, 2.5811, 2.0541, 0.0026) )

BSfactor(x)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("BSfactor", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("forestplot")
### * forestplot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: forestplot
### Title: Forest plot
### Aliases: forestplot

### ** Examples

x <- pmp.update( beta = c(0.0126, 5.0052, 1.2976, 0.0005),
       se.beta = c(0.050, 2.581, 2.054, 0.003) )
forestplot(x)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("forestplot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.PMP")
### * plot.PMP

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.PMP
### Title: Plot of prior, data, and posterior distributions
### Aliases: plot.PMP

### ** Examples

x <- pmp(beta = 5.005, se.beta = 2.05)
plot(x)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.PMP", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.PMPlist")
### * plot.PMPlist

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.PMPlist
### Title: Plot of posterior model probabilities
### Aliases: plot.PMPlist

### ** Examples

x <- pmp.update(beta = c(0.0126, 5.0052, 1.2976, 0.0005),
       se.beta = c(0.050, 2.581, 2.054, 0.003) )
plot(x)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.PMPlist", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pmp")
### * pmp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pmp
### Title: Calculates posterior model probabilities for one study
### Aliases: pmp

### ** Examples

# library(labstats) # need to install separately
# plot(time.immob ~ dose, data=fluoxetine) 
# summary(lm(time.immob ~ dose, data=fluoxetine))
x <- pmp(beta=-0.25200, se.beta=0.09913) # dose effect from above output
x



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pmp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pmp.update")
### * pmp.update

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pmp.update
### Title: Calculates posterior model probabilities for multiple studies
### Aliases: pmp.update

### ** Examples

x <- pmp.update(beta = c(0.0126, 5.0052, 1.2976, 0.0005),
       se.beta = c(0.050, 2.581, 2.054, 0.003) )
x
plot(x)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pmp.update", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
