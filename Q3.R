setwd("C:/Users/BJXY/Desktop/lcq/R")
library(xlsx)
source("jiou.R")
source("swap.R")
source("tiaojian.R")
source("chushi.R")
source("fenpei.R")

time.start <- 0
time.total <- 8*3600
startp <- 1
# 选定第N组
cat("请选择第几组数据（1~3）\n")
N <- scan("")# N <- c(1,2,3)
th <- matrix(0,nrow = 400,ncol = 5)
tt <- rep(0)

data <- tiaojian(N)
time.cnc <- data$cnc;time.rgv <- data$rgv
time.wash <- data$wash;th <- data$th
tt <- data$time;time.move <- data$move

cnc.id <- matrix(c(1,2,3,6,8,7,5,4,
                   1,2,3,7,8,6,5,4,
                   1,3,5,7,8,6,4,2),
                 nrow = 3,byrow = T)

set.seed(50)
a <- c(0,1)
for (i in 1:400) {
  gz <- sample(1:100,1)
  if(gz==1){
    a[i] <- 1
  }else{
    a[i] <- 0
  }
}
  
for (i in 2:8) {
  th <- chushi(i,cnc.id[N,])
}

L <- 8
ck <- matrix(nrow = L,ncol = 5)
ck <- th[1:8,]
gzid <- 0
gzidd <- 0
gzstart <- 0
gzend <- 0
source("Sliding_window.R")
i <- 9
repeat{
  leave <- which.min(ck[,5])
  ck.temp <- ck[leave,]
  ck <- ck[-leave,]
  id <- ck.temp[2]
  ttt <- jiou(id,N)
  ttl <- jiou(ck[which.max(ck[,2]),2],N)
  st <- ck.temp[4]
  if(a[i]==1){
    pt <- st+ttt*2+15*60
    gzid <- cbind(gzid,id)
    gzidd <- cbind(gzidd,i)
    gzstart <- cbind(gzstart,st+ttt)
    gzend <- cbind(gzend,pt)
  }else{
    pt <- st+time.cnc[1,N]+ttt
  }
  pnow <- ceiling(id/2)
  pforward <- ceiling(ck[which.max(ck[,4]),2]/2)
  mt <- time.move[pnow,pforward]
  maxt <- max(pt,ttl+time.wash[1,N]+ck[which.max(ck[,4]),])
  
  at <- mt+maxt
  th[i,] <- c(i,id,st,at,pt)
  ck <- rbind(ck,th[i,])
  
  th[i,] <- ck[8,]
  fin.time <- th[i,4]+jiou(th[i,2],N)+time.wash[1,N]
  if(fin.time>=time.total){
    break
  }else
    i <- i+1
}
number <- max(th[,1])

gzdata <- rbind(gzidd,gzid,gzstart,gzend)
gzdata <- t(gzdata[,2:5])
cat("第",N,"种情况8小时加工零件",number,"件")
library(xlsx)
file <- c("data31.xls","data32.xls","data33.xls")
file2 <- c("gz1.xls","gz2.xls","gz3.xls")
write.xlsx(gzdata,file2[N],
col.names = F,row.names = F)
write.xlsx(th[1:number,1:4],file[N],
col.names = F,row.names = F)

