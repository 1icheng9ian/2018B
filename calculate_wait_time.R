setwd("C:/Users/BJXY/Desktop/lcq/R")

load("1.Rdata")
f1 <- out
load("2.Rdata")
f2 <- out
load("3.Rdata")
f3 <- out
rm(out)
time <- 0
total <- 8*3600
maxt <- c(102,124,103)
wait <- rep(0,3)
pre <- rep(0,3)
for (N in 1:3) {
  if(N==1){
    data <- f1
  }else if(N==2){
    data <- f2
  }else if(N==3){
    data <- f3
  }
  len <- nrow(data)-1
  for (i in 1:len) {
    deltat <- data[i+1,3]-data[i,3]
    if(deltat>maxt[N]){
      wait[N] <- sum(wait[N],deltat)
    }
  } 
  pre[N] <- 1-wait[N]/total
  cat("第",N,"组作业效率为",pre[N],"\n")
}






