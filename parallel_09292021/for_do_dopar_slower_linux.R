library(doMC)
num.of.core <-  detectCores()
registerDoMC(num.of.core)
I <- 10^3L


##==========================
##  3 functions
##==========================
for.loop <- function(I) {
  out <- double(I)
  for (i in seq_len(I))
    out[i] <- sqrt(i)
  out
}

foreach.do <- function(I) {
  out <- foreach(i = seq_len(I), .combine=c) %do%
    sqrt(i)
  out
}

foreach.dopar <- function(I) {
  out <- foreach(i = seq_len(I), .combine=c) %dopar%
    sqrt(i)
  out
}
identical(for.loop(I), foreach.do(I), foreach.dopar(I))
## [1] TRUE


library(rbenchmark)
benchmark(for.loop(I), foreach.do(I), foreach.dopar(I))

#test             replications   elapsed relative user.self sys.self user.child sys.child
#1      for.loop(I)          100   0.100     1.00     0.100    0.000      0.000     0.000
#2    foreach.do(I)          100  25.893   258.93    25.852    0.031      0.000     0.000
#3 foreach.dopar(I)          100  32.675   326.75    23.306    8.859     11.065    55.619


