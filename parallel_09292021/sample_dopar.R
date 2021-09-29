avec <- c(1:200)
bvec <- c(1:400)
library(doParallel)
number.of.workers <- max (1, (detectCores() - 1))
cl <- makeCluster(number.of.workers, type='PSOCK')
registerDoParallel(cl)

x <-
  foreach(b=bvec, .combine='cbind') %:%
  foreach(a=avec, .combine='c') %dopar% {
     sim(a, b)
   } 
x

stopCluster(cl)


sim <- function (a, b)
{
  return (10*a + b)
}
x <- matrix(0, length(avec), length(bvec))
y <- matrix(0, length(avec), length(bvec))


y <-
  foreach(a=avec, .combine='rbind') %:%
  foreach(b=bvec, .combine='c') %dopar% {
    sim(a, b)
  } 
y


y <- matrix(0, length(avec), length(bvec))


for (k in 11:11){     
  # thePhenotypeNum  Just one phenotype being tested here for now.  I was going to loop through all the phenotypes.  
  #11 in my phenotype data file is the hyperoxia total cell count data
  
  thePhenotypeData <- phenotypeData[,k]
  myExport = c( "kinship_matrix", "id", "thePhenotypeNum", "corSymm", "recode", "Initialize", "myCounterBadSNPsMito", "myCounterBadSNPsNuc", "myCounterBadCorr", "lme", "interactNbyM") #you have to export functions and packages that each processor will use.
  
  thepValues <-foreach( idx = 1:numOfFiles, 
                        .export = myExport, 
                        .packages=c('car', 'lme4qtl','nlme', 'coxme'), 
                        .combine=rbind) %dopar% { 
                          #numOfFiles is the number of the files which is one for each chr.
                           interactNbyM(idx)     
                          #this is the function that you have to create that does your interaction test by chr.  In it, return the matrix that the function tallies the p-values for each chr.  i.e. return(myOutVariable)  The parallel function will then combine the matrices by row.  In other words, each of the 19 chrs pvalues matrices (myOutVariable) will be combined by rows in the end into thepValues final matrix.
                }
  
}


qsort <- function(x) {
  n <- length(x)
  if (n == 0) {
    x
  } else {
    p <- sample(n, 1)
    smaller <- foreach(y=x[-p], .combine=c) %:% when(y <= x[p]) %do% y
    larger  <- foreach(y=x[-p], .combine=c) %:% when(y >  x[p]) %do% y
    c(qsort(smaller), x[p], qsort(larger))
  }
}

qsort.parallel <- function(x) {
  n <- length(x)
  if (n == 0) {
    x
  } else {
    p <- sample(n, 1)
    smaller <- foreach(y=x[-p], .combine=c) %:% when(y <= x[p]) %dopar% y
    larger  <- foreach(y=x[-p], .combine=c) %:% when(y >  x[p]) %dopar% y
    c(qsort(smaller), x[p], qsort(larger))
  }
}




system.time(
  qsort(runif(1000))
)


#number.of.workers <- max (1, (detectCores() - 1))
#cl <- makeCluster(number.of.workers, type='PSOCK')
registerDoParallel(cl)
system.time(
  qsort.parallel(runif(1000))
)
registerDoSEQ()


