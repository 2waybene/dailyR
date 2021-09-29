

##  When parallel is not necessary
#number of iterations in the loop
iters<-10

#vector for appending output
ls<-vector('list',length=iters)

#start time
strt<-Sys.time()

#loop
for(i in 1:iters){
  
  #counter
  cat(i,'\n')
  
  to.ls<-rnorm(1e6)
  to.ls<-summary(to.ls)
  
  #export
  ls[[i]]<-to.ls
  
}

#end time
print(Sys.time()-strt)
# Time difference of 1.70717 secs

##=================================
##  When parallel is  necessary
##=================================
#iterations to time
iters<-seq(10,100,by=10)

#output time vector for  iteration sets
times<-numeric(length(iters))

#loop over iteration sets
for(val in 1:length(iters)){
  
  cat(val,' of ', length(iters),'\n')
  
  to.iter<-iters[val]
  
  #vector for appending output
  ls<-vector('list',length=to.iter)
  
  #start time
  strt<-Sys.time()
  
  #same for loop as before
  for(i in 1:to.iter){
    
    cat(i,'\n')
    
    to.ls<-rnorm(1e6)
    to.ls<-summary(to.ls)
    
    #export
    ls[[i]]<-to.ls
    
  }
  
  #end time
  times[val]<-Sys.time()-strt
  
}

#plot the times
library(ggplot2)

to.plo<-data.frame(iters,times)
ggplot(to.plo,aes(x=iters,y=times)) + 
  geom_point() +
  geom_smooth() + 
  theme_bw() + 
  scale_x_continuous('No. of loop iterations') + 
  scale_y_continuous ('Time in seconds')


#predict times
mod<-lm(times~iters)
predict(mod,newdata=data.frame(iters=1e4))/60
# 23.43049


#import packages
library(foreach)
library(doParallel)

#number of iterations
iters<-1e4

#setup parallel backend to use 8 processors
cl<-makeCluster(44)
registerDoParallel(cl)

#start time
strt<-Sys.time()

#loop
ls<-foreach(icount(iters)) %dopar% {
  
  to.ls<-rnorm(1e6)
  to.ls<-summary(to.ls)
  to.ls
  
}

print(Sys.time()-strt)
stopCluster(cl)



