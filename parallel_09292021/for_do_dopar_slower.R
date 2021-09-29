library(parallel)
library(foreach)
library(doParallel)
number.of.workers <- max (1, (detectCores() - 1))
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


cl <- makeCluster(number.of.workers)
registerDoParallel(cl)
identical(for.loop(I), foreach.do(I), foreach.dopar(I))
stopCluster(cl)
## [1] TRUE

library(rbenchmark)
cl <- makeCluster(number.of.workers)
registerDoParallel(cl)
benchmark(for.loop(I), foreach.do(I), foreach.dopar(I))
stopCluster(cl)
