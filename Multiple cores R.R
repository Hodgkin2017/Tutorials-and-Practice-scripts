###################
### Using multiple cores with R
###################

##See website: http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/

library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1
no_cores

# Initiate cluster
cl <- makeCluster(no_cores)
cl

lapply(1e6:2.5e7,function(exponent) 2^exponent)


parLapply(cl, 1e6:2.5e7,
          function(exponent)
            2^exponent
)

stopCluster(cl)

#########
##Include objects in cluster:
cl<-makeCluster(no_cores)
##base not part of environment:
base <- 2

parLapply(cl, 
          2:4, 
          function(exponent) 
            base^exponent)

stopCluster(cl)

##base part of environment
cl<-makeCluster(no_cores)

base <- 2
clusterExport(cl, "base")
parLapply(cl, 
          2:4, 
          function(exponent) 
            base^exponent)

stopCluster(cl)

##Import R library to parLapply:

clusterEvalQ(cl, library(rms))

###########
##parSapply

cl <- makeCluster(no_cores)
cl

parSapply(cl, 2:4,
          function(exponent)
            2^exponent
)

stopCluster(cl)

#Matrix output with names (this is why we need the as.character):

cl <- makeCluster(no_cores)
cl
clusterExport(cl, "base")
parSapply(cl, as.character(2:4), 
          function(exponent){
            x <- as.numeric(exponent)
            c(base = base^x, self = x^x)
          })

stopCluster(cl)

##########
#foreach package

library(foreach)
library(doParallel)

cl<-makeCluster(no_cores)
registerDoParallel(cl)

#If use: registerDoParallel(no_cores)
#the use: stopImplicitCluster()
#clusterExport(cl, "base")
#stopCluster()

foreach(exponent = 2:4, 
        .combine = c)  %dopar%  
  base^exponent

foreach(exponent = 2:4, 
        .combine = rbind)  %dopar%  
  base^exponent

foreach(exponent = 2:4, 
        .combine = list,
        .multicombine = TRUE)  %dopar%  
  base^exponent

foreach(exponent = 2:4, 
        .combine = list)  %dopar%  
  base^exponent

#######
#Local environment variables available:
  
base <- 2
cl<-makeCluster(2)
cl
registerDoParallel(cl)
foreach(exponent = 2:4, 
        .combine = c)  %dopar%  
  base^exponent
stopCluster(cl)

##does not work as base not available in local environment:
test <- function (exponent) {
  foreach(exponent = 2:4, 
          .combine = c)  %dopar%  
    base^exponent
}
test()

##.export loads local variables:
base <- 2
cl<-makeCluster(2)
registerDoParallel(cl)

base <- 4
test <- function (exponent) {
  foreach(exponent = 2:4, 
          .combine = c,
          .export = "base")  %dopar%  
    base^exponent
}
test()

stopCluster(cl)

##For exporting packages into foreach:
.packages = c("rms", "mice")

###########
##foreach speed test:

system.time(lapply(c(1e100000, 1e100000, 1e100000, 1e100000, 1e100000, 1e100000, 1e100000),function(exponent) 
  sd(sd(sd(log(mean(log(log((log(log(1e100^exponent)/exponent*36)/exponent))))))))))


cl<-makeCluster(7)
registerDoParallel(cl)

base <- 2
test <- function (exponent) {
  foreach(exponent = c(1e100000, 1e100000, 1e100000, 1e100000, 1e100000, 1e100000, 1e100000),
                  .combine = list,
                  .multicombine = TRUE,
          .export = "base")  %dopar%  
    sd(sd(sd(log(mean(log(log((log(log(1e100^exponent)/exponent*36)/exponent))))))))
}
system.time(test())

stopCluster(cl)

#foreach = very slow!!!!

#############
##FORK is more memory efficient
cl<-makeCluster(no_cores, type="FORK")

############
##Another option
##This one seems to work!!!!!!!!

system.time(test1<- lapply(1:1000, function(i) sqrt(1/(sin(i))^2)-sum(rnorm(10^6))))

install.packages("doMC")

library(doMC)
options(cores = 7)
registerDoMC()

my.function<- function(){
  data=vector(length=1000)
data=foreach(i=1:1000) %dopar% {
  sqrt(1/(sin(i))^2)-sum(rnorm(10^6))
}
}
system.time(test<- my.function())
test
test=unlist(test)
rm(test)

##############
## Look up mclapply....

#https://www.r-bloggers.com/a-no-bs-guide-to-the-basics-of-parallelization-in-r/

system.time({
  doors <- 1:3
  runs <- 1e6
  game.outputs <- numeric(runs)
  for (run in 1:runs){
    prize.door <- sample(doors, size=1)
    choice <- sample(doors, size=1)
    
    if (choice!=prize.door) game.outputs[run] <- 1 # Always switch
  }
  avg <- mean(game.outputs)
})[3]
#Takes abbout 47s
avg

##Multicore:
#########################################
# ---------------------------------------
# Functions
# ---------------------------------------
#########################################

# One simulation of the Monty Hall game
onerun <- function(.){ # Function of no arguments
  doors <- 1:3
  prize.door <- sample(doors, size=1)
  choice <- sample(doors, size=1)
  
  if (choice==prize.door) return(0) else return(1) # Always switch
}

# Many simulations of Monty Hall games
MontyHall <- function(runs, cores=detectCores()){
  require(parallel)
  # clusterApply() for Windows
  if (Sys.info()[1] == "Windows"){
    cl <- makeCluster(cores)
    runtime <- system.time({
      avg <- mean(unlist(clusterApply(cl=cl, x=1:runs, fun=onerun)))
    })[3]
    stopCluster(cl) # Don't forget to do this--I frequently do
    
    # mclapply() for everybody else
  } else {
    runtime <- system.time({
      avg <- mean(unlist(mclapply(X=1:runs, FUN=onerun, mc.cores=cores)))
    })[3]
  }
  return(list(avg=avg, runtime=runtime))
}

#########################################
# ---------------------------------------
# Outputs
# ---------------------------------------
#########################################

run1 <- rbind(c(MontyHall(1e6, cores=1), "cores"=1))
run2 <- rbind(c(MontyHall(1e6, cores=4), "cores"=4))
run3 <- rbind(c(MontyHall(1e6, cores=8), "cores"=8))
rbind(run1, run2, run3)




########################
#From Henrik: Sample code looks like this:
library(parallel)
rm(list=ls()) # Remove old environment
cl = makeCluster(detectCores()-1) # Set number of cores
clusterEvalQ(cl, require(repsych))
clusterExport(cl, as.list(ls())) # Export all variables

# Actual usage
results <- parLapply(cl, input, function(ii, ...) your.function(ii))
stopCluster(cl) # Kill the cluster




##Parrallising R using bash:

#!/bin/sh

## This script runs the simulations in R in parallel

# run () {
#   START=$1
#   END=$2
#   for ii in $(seq $START $END); do
#   ( 
#     RETURN_VAL=$(nice -n 5 Rscript --vanilla /local/data/public/hpa22/assignments/syba1/gillespie.R
#     ) &
#       done
# }
# 
# run 1 20
# wait
# run 21 40
# wait
# run 41 60
# wait
# run 61 80
# wait
# run 81 100
# 
# exit

##########

#On Linux there is also the GNU parallel command, which is super efficient, but somewhat tricky to use.
#The best reference is probably the official:
#  https://www.gnu.org/software/bash/manual/html_node/GNU-Parallel.html
