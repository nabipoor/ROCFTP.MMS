library(testthat)
library(ROCFTP.MMS)

expect_equal(ROCFTP.MMS(100,c(20,40), function(x) {dnorm(x, mean=30, sd=1, log=TRUE)}, 0.5, log=FALSE),
             noquote("Error: Change length of coalescence block or sigma or log parameter"))
expect_equal(ROCFTP.MMS("a",c(20,40), function(x) {dnorm(x, mean=30, sd=1, log=TRUE)}, 0.5, log=TRUE),
            noquote("Error: LB should be an integer"))
expect_equal(ROCFTP.MMS(-10,c(20,40), function(x) {dnorm(x, mean=30, sd=1, log=TRUE)}, 0.5, log=TRUE),
             noquote("Error: LB should be an integer"))
expect_equal(ROCFTP.MMS(100,c(20,40), function(x) {dnorm(x, mean=30, sd=1, log=TRUE)}, -0.5, log=TRUE),
             noquote("Error: sigma should be a positive number"))
expect_equal(ROCFTP.MMS(100,c(20,40,5), function(x) {dnorm(x, mean=30, sd=1, log=TRUE)}, 0.5, log=TRUE),
             noquote("Error: The length of start vector should be two"))
