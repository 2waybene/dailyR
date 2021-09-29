##=================================================================================================
##  Credit: https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf
##  multiCore_CrossPlatforms.R
##  Author: Jianying Li
##=================================================================================================


##================================================
##  Windows, use doParallel package
##================================================


if( Sys.info()[['sysname']] == 'Windows' ){
  message(paste(
    "\n", 
    "   *** Microsoft Windows detected ***\n",
    "   \n",
    "   Use doSNOW package for multicore ability\n\n"))
  x <- iris[which(iris[,5] != "setosa"), c(1,5)]
  trials <- 10000
  library(doParallel)
  number.of.workers <- max (1, (detectCores() - 1))
  cl <- makeCluster(number.of.workers, type='PSOCK')
  registerDoParallel(cl)
  
  ptime <- system.time({
    r <- foreach(icount(trials), .combine=cbind) %dopar% {
      ind <- sample(100, 100, replace=TRUE)
      result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
      coefficients(result1)
    }
  })[3]
  stopCluster(cl)
  registerDoSEQ()
  stime <- system.time({
    r <- foreach(icount(trials), .combine=cbind) %do% {
      ind <- sample(100, 100, replace=TRUE)
      result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
      coefficients(result1)
    }
  })[3]
  
  cat(sprintf('Sequential time: %f\n', stime))
  cat(sprintf('Speed up for %d workers: %f\n',
              number.of.workers, round(stime / ptime, digits=2)))
  
}

##================================================
##  Linux, 
##  use doMC package
##  doParallel also works under Linux but not 
##  as good as doMC
##================================================

if( Sys.info()[['sysname']] == 'Linux' ){
  message(paste(
    "\n", 
    "   *** Linux detected ***\n",
    "   \n",
    "   Use doMC package for multicore ability\n\n"))
  x <- iris[which(iris[,5] != "setosa"), c(1,5)]
  trials <- 10000
  library(doMC)
  num.of.core <-  detectCores()
  registerDoMC(num.of.core)


  ptime <- system.time({
    r <- foreach(icount(trials), .combine=cbind) %dopar% {
      ind <- sample(100, 100, replace=TRUE)
      result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
      coefficients(result1)
    }
  })[3]

  cat(sprintf('Parallel time using doMC on %d workers: %f\n',
              num.of.core, ptime))

  stime <- system.time({
    r <- foreach(icount(trials), .combine=cbind) %do% {
      ind <- sample(100, 100, replace=TRUE)
      result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
      coefficients(result1)
    }
  })[3]

  cat(sprintf('Sequential time: %f\n', stime))
  
  cat(sprintf('Speed up for %d workers: %f\n',
            getDoParWorkers(), round(stime / ptime, digits=2)))

}



##================================================
##  MacOS
##  Will test both packages: doParallel vs. doMC
##  doMC works better than doParallel
##================================================
if( Sys.info()[['sysname']] == 'Darwin' ){
  message(paste(
    "\n", 
    "   *** MacOS detected ***\n",
    "   \n",
    "   Use doMC package for multicore ability\n\n"))
  
  x <- iris[which(iris[,5] != "setosa"), c(1,5)]
  trials <- 10000
  library(doMC)
  num.of.core <-  detectCores()
  registerDoMC(num.of.core)
  ptime <- system.time({
    r <- foreach(icount(trials), .combine=cbind) %dopar% {
      ind <- sample(100, 100, replace=TRUE)
      result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
      coefficients(result1)
    }
  })[3]
  
  
  cat(sprintf('Parallel time using doMC on %d workers: %f\n',
              num.of.core, ptime))
  
  stime <- system.time({
    r <- foreach(icount(trials), .combine=cbind) %do% {
      ind <- sample(100, 100, replace=TRUE)
      result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
      coefficients(result1)
    }
  })[3]
  
  cat(sprintf('Sequential time: %f\n', stime))
  cat(sprintf('Speed up for %d workers: %f\n',
              num.of.core, round(stime / ptime, digits=2)))
  
}


