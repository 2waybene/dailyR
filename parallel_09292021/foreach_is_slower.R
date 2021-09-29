##===============================================================
##  File name: foreach_is_slower.R
##  Author: Jianying Li
##  Sample file: genoJenData.mitoCarta.TAGstered.in.mito.snp
##===============================================================

##  Two functions doing the same process, but foreach is 
##  almost 20 times slower than "for" loop!!

##  Parallel function using foreach -- dopar
filter.mono.morphic.mod.2.dopar <- function (in.matrix, cols2work)
{
  snp.2.exclude <- foreach (i = 1:dim(in.matrix)[1], 
              .combine = 'c') %dopar%
          {
            if (length(unique(as.character(as.vector(in.matrix [i,cols2work])))) ==1 )
            {
              i
            }
          }
  return(snp.2.exclude)
}

##  Serielized function with foreach
filter.mono.morphic.mod.2.do <- function (in.matrix, cols2work)
{
  snp.2.exclude <- foreach (i = 1:dim(in.matrix)[1], 
                            .combine = 'c') %do%
                            {
                              if (length(unique(as.character(as.vector(in.matrix [i,cols2work])))) ==1 )
                              {
                                i
                              }
                            }
  return(snp.2.exclude)
}
##  Serielized function with for loop
filter.mono.morphic.mod.2.for <- function (in.matrix, cols2work)
{
  snp.2.exclude <- c()
  index = 1
  for (i in 1:dim(in.matrix)[1])
  {
    if (length(unique(as.character(as.vector(in.matrix [i,cols2work])))) ==1 )
    {
      snp.2.exclude[index] = i
      index = index + 1
    }
  }
  return(snp.2.exclude)
}
##===============================================================================

load("x:/learningR/learningParallel/foreach_sample_dm.rda")
library(doParallel)
number.of.workers <- max (1, (detectCores() - 1))


ptm <- proc.time()
  num.for <- filter.mono.morphic.mod.2.for (genoJenData.mitoCarta.TAGstered.in.mito.snp , c(17:47))
proc.time() - ptm
#user  system elapsed 
#5.67    0.00   5.68 

ptm <- proc.time()
  num.do <- filter.mono.morphic.mod.2.do (genoJenData.mitoCarta.TAGstered.in.mito.snp , c(17:47))
proc.time() - ptm
#user  system elapsed 
#7.71    0.00   7.71 

cl <- makeCluster(number.of.workers)
registerDoParallel(cl)
ptm <- proc.time()
  num.dopar <- filter.mono.morphic.mod.2.dopar (genoJenData.mitoCarta.TAGstered.in.mito.snp , c(17:47))
proc.time() - ptm
#user  system elapsed 
#14.30    4.40   91.07
stopCluster(cl)

identical(num.for, num.do, num.dopar)
#[1] TRUE
