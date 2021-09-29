##==================================================================================
# testParalleSugar.R
# Originally from Nathan VanHoudnos
#https://github.com/nathanvan/parallelsugar
#http://edustatistics.org/nathanvan/2014/07/14/implementing-mclapply-on-windows/
##=================================================================================



library(parallelsugar)

system.time( mclapply(1:4, function(xx){ Sys.sleep(10) }) )

library(Matrix)


## Define a global variable
a.global.variable <- Matrix::Diagonal(3)


## Define a global function 

wait.then.square <- function(xx)
  {
     ## Wait for 5 seconds
     Sys.sleep(5);
     ## Square the argument
    xx^2 
  }


## Check that it works with plain lapply
 
 
system.time( 
serial.output <- lapply( 1:4, function(xx) {
         return( wait.then.square(xx) + a.global.variable ) }) 

)

## Test with the modified mclapply  
system.time(
par.output <- mclapply( 1:4, function(xx) {
         return( wait.then.square(xx) + a.global.variable )
     })
)

## Are they equal? 
all.equal( serial.output, par.output )
 
 
 