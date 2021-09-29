##==================================
##  R_parallel_JYL.R
##  Credit goes to Henry Scharf
##==================================
setwd("~/myGit/dailyRLearning/parallel_09292021/")


load(url("http://www.stat.colostate.edu/~scharfh/CSP_parallel/data/arrivals_subset.RData"))
K <- 50
N <- dim(arrivals.sub)[1]

## for convenience kill off 8 observations (we have 5208) and make cv test sets
set.seed(1985)
discarded <- sample(1:N, size = 8)
cv.test.sets <- matrix(sample((1:N)[-discarded], size = N - 8), ncol = K)

source("helpScripts/fitting_functions.R")

##===========================
##  a "for" version
##===========================
err.for <- NULL
system.time(
  for (i in 1:K) {
    err.for <- cbind(err.for, get.errs(test.set = cv.test.sets[, i],
                                       discarded = discarded,
                                       q = 1))
  }
)

##===========================
## apply version
##===========================
system.time(
  err.apply <- sapply(X = 1:K, 
                      FUN = function(i) {
                        get.errs(test.set = cv.test.sets[, i],
                                 discarded = discarded,
                                 q = 1)
                      }
  )
)

##===========================
##  a "foreach" version
##===========================
## foreach version
library(doParallel)
library(parallel)
cl <- max (1, (detectCores() - 1))
registerDoParallel(cl)
#registerDoParallel(cl = 80)
system.time(
  err.foreach <- foreach(i=1:K,
                         .inorder = FALSE,
                         .combine = "cbind",
                         .packages = "splines") %dopar% {
                           get.errs(test.set = cv.test.sets[, i],
                                    discarded = discarded,
                                    q = 1)
                         }
)
stopImplicitCluster()

##  Or, try to register cluster then stop it
##  Do not seem to work though
cl <- makeCluster((detectCores() - 1 ), type='PSOCK')
system.time(
  err.foreach <- foreach(i=1:K,
                         .inorder = FALSE,
                         .combine = "cbind",
                         .packages = "splines") %dopar% {
                           get.errs(test.set = cv.test.sets[, i],
                                    discarded = discarded,
                                    q = 1)
                         }
)
stopCluster(cl)


##===========================
##  a "foreach" version
##  under Linux with doMC
##===========================
library(doMC)
num.of.core <-  detectCores()

registerDoMC(num.of.core)
#registerDoMC(4)
system.time(
  err.foreach <- foreach(i=1:K,
                         .inorder = FALSE,
                         .combine = "cbind",
                         .packages = "splines") %dopar% {
                           get.errs(test.set = cv.test.sets[, i],
                                    discarded = discarded,
                                    q = 1)
                         }
)












