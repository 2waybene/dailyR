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

load("~/learningR/learningParallel/foreach_sample_dm.rda")
library(doMC)
num.of.core <-  detectCores()
registerDoMC(num.of.core)


ptm <- proc.time()
  num.for <- filter.mono.morphic.mod.2.for (genoJenData.mitoCarta.TAGstered.in.mito.snp , c(17:47))
proc.time() - ptm
#user  system elapsed 
#6.127   0.163   6.293

ptm <- proc.time()
  num.do <- filter.mono.morphic.mod.2.do (genoJenData.mitoCarta.TAGstered.in.mito.snp , c(17:47))
proc.time() - ptm
#user  system elapsed 
#11.078   0.258  11.340 


ptm <- proc.time()
  num.dopar <- filter.mono.morphic.mod.2.dopar (genoJenData.mitoCarta.TAGstered.in.mito.snp , c(17:47))
proc.time() - ptm
#user  system elapsed 
# 6.309   5.176   2.719 


identical(num.for, num.do, num.dopar)
#[1] TRUE
