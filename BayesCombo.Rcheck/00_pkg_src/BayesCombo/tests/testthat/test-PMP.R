context("BayesCombo")

test_that("temptest", {
              a <- 5
              b <- 5
              expect_that(a, equals(b)) } )

## test_that("PMP", {
              
##               x <- pmp.update(beta = c(0.090, 0.140, 1.090, 1.781),
##                               se.beta = c(0.000841, 0.002916, 0.008649, 0.032041)^0.5,
##                               beta0 = 0, mod.priors = rep(1/3, 3))
              
##               testpmp <- c(0.021816130, 0.975347136, 0.002836733)
              
##               testpmp <- matrix( c(0.021816130, 1.709809e-03, 6.203356e-32, 3.654112e-52
##                                  , 0.975347136, 9.982656e-01, 1.000000e+00, 1.000000e+00
##                                  , 0.002836733, 2.459508e-05, 0.000000e+00, 0.000000e+00),
##                                 nrow = 3, byrow = TRUE)
              
##               colnames(testpmp) <- colnames(x$pmps)
##               rownames(testpmp) <- rownames(x$pmps)
              
##               expect_that(x$pmp, equals(testpmp))
              
##           }
          
##   )

## test_that("BS", {
              
##               x <- BSfactor(beta = c(0.068, -0.084, 0.175, 0.337),
##                             se.beta = (c(0.000841, 0.002916, 0.008649, 0.032041)^0.5),
##                             beta0 = 0 )
              
              
##               testpmp <- matrix( c(0.04136464, 0.9579635, 0.0006718938,
##                                    0.04412399, 0.9552575, 0.0006184713),
##                                 ncol  = 3, byrow = TRUE)
              
##               colnames(testpmp) <- colnames(x$PMP)
              
##               expect_that(round(x$PMP[1:2, ], 7), equals(round(testpmp, 7)))
              
##           }
          
##           )
