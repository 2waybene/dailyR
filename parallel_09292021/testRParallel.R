require(parallel) 

## On Windows, the following line will take about 40 seconds to run
## because by default, mclapply is implemented as a serial function
## on Windows systems.
system.time( mclapply(1:4, function(xx){ Sys.sleep(10) }) )
##    user  system elapsed 
##    0.00    0.00   40.06 

## Using the ideas developed in this post, we can implement
## a parallel (as it should be!) mclapply() on Windows. 
#source("http://www.stat.cmu.edu/~nmv/setup/mclapply.hack.R")
source("X:/learningR/learningParallel/mclapply_hack_funcs.R")
## 
##     *** Microsoft Windows detected ***
##     
##     For technical reasons, the MS Windows version of mclapply()

wait.then.square <- function(xx){
  # Wait for one second
  Sys.sleep(1);
  # Square the argument 
  xx^2 } 
#If we want to run it on the integers from 1 to 4 in serial, it will take about 4 seconds

## Run in serial 
system.time( serial.output <- lapply( 1:4, wait.then.square ) )

system.time( par.output   <- mclapply( 1:4, wait.then.square,
                                     mc.cores=4             ) )




require(Matrix)
( a.global.variable <- Diagonal(3) )


mclapply.hack <- function(...) {
  ## Create a cluster
  ## ... How many workers do you need?
  ## ... N.B. list(...)[[1]] returns the first 
  ##          argument passed to the function. In
  ##          this case it is the list to iterate over
  size.of.list <- length(list(...)[[1]])
  cl <- makeCluster( min(size.of.list, detectCores()) )
  
  ## Find out the names of the loaded packages 
  loaded.package.names <- c(
    ## Base packages
    sessionInfo()$basePkgs,
    ## Additional packages
    names( sessionInfo()$otherPkgs ))
  
  ## N.B. tryCatch() allows us to properly shut down the 
  ##      cluster if an error in our code halts execution
  ##      of the function. For details see: help(tryCatch)
  tryCatch( {
    
    ## Copy over all of the objects within scope to
    ## all clusters. 
    ## 
    ## The approach is as follows: Beginning with the 
    ## current environment, copy over all objects within
    ## the environment to all clusters, and then repeat
    ## the process with the parent environment. 
    ##
    this.env <- environment()
    while( identical( this.env, globalenv() ) == FALSE ) {
      clusterExport(cl,
                    ls(all.names=TRUE, env=this.env),
                    envir=this.env)
      this.env <- parent.env(environment())
    }
    ## repeat for the global environment
    clusterExport(cl,
                  ls(all.names=TRUE, env=globalenv()),
                  envir=globalenv())
    
    ## Load the libraries on all the clusters
    ## N.B. length(cl) returns the number of clusters
    parLapply( cl, 1:length(cl), function(xx){
      lapply(loaded.package.names, function(yy) {
        ## N.B. the character.only option of 
        ##      require() allows you to give the 
        ##      name of a package as a string. 
        require(yy , character.only=TRUE)})
    })
    
    ## Run the lapply in parallel 
    return( parLapply( cl, ...) )
  }, finally = {        
    ## Stop the cluster
    stopCluster(cl)
  })
}

#We can test it as follows:
  
system.time( serial.output <- lapply( 1:4,  function(xx) {
                                            return( wait.then.square(xx) + a.global.variable )})
           
           ) 




require(parallel)
require(Matrix)
##
## Define example function and the global variable 
wait.then.square <- function(xx){
  # Wait for one second
  Sys.sleep(1);
  # Square the argument 
  xx^2 } 
a.global.variable <- Diagonal(3) 

serial.output <- lapply( 1:4,
                         function(xx) {
                           return( wait.then.square(xx) + a.global.variable )
                         })
cl <- makeCluster( 4 )



par.setup <- parLapply( cl, 1:length(cl),
                        function(xx) {
                          require(Matrix) 
                        })

## Step 3: Distribute the necessary R objects 
clusterExport( cl, c('wait.then.square', 'a.global.variable') )


par.output <- parLapply(cl, 1:4,
                        function(xx) {
                          return( wait.then.square(xx) + a.global.variable )
                        })

stopCluster(cl)


all.equal(serial.output, par.output)




