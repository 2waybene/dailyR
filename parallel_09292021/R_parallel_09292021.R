library(     "parallel")
             library("foreach")
             library("doParallel")

numCores <- detectCores()/2 
cl <<- parallel::makeCluster(numCores)
doParallel::registerDoParallel(cl)




wgs <- NULL
registerDoMC(num.of.core)
system.time({ 
  
  wgs <- foreach(i = 1:2, .combine = 'rbind') %dopar% {  # Dummy variable to do parallel upon
#for (i in seq(1,2)){
  boot <- bootstrapSigExposures(humandata[,i]/sum(humandata[,i]), sigtest, R = 10000, mutation.count = sum(humandata[,i]))
  signature_fractionhdpT <- apply(as.matrix(boot$exposures),1,function(x) 1-(1+length(which(x>0.05)))/10001)
 # wgs[i,] <- signature_fractionhdpT
}

})

wgs.parallel <- wgs
##  serialized method

wgs <- matrix(0,4645,ncol=48)
system.time({ 
   for (i in seq(1,2)){
       boot <- bootstrapSigExposures(humandata[,i]/sum(humandata[,i]), sigtest, R = 10000, mutation.count = sum(humandata[,i]))
       signature_fractionhdpT <- apply(as.matrix(boot$exposures),1,function(x) 1-(1+length(which(x>0.05)))/10001)
       wgs[i,] <- signature_fractionhdpT
     }
})


