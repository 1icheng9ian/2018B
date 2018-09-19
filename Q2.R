setwd("C:/Users/BJXY/Desktop/lcq/R")
source("jiou.R")
# source("swap.R")
source("tiaojian.R")
# source("chushi.R")
# source("fenpei.R")
time.start <- 0
time.total <- 8*3600
startp <- 1
cat("请选择第几组数据（1~3）\n")
N <- scan("")# N <- c(1,2,3)
th <- matrix(0,nrow = 400,ncol = 5)
th2 <- matrix(0,nrow = 400,ncol = 4)
tt <- rep(0)

data <- tiaojian(N)
cnc <- data$cnc[2:3,N]
wash <- data$wash[N];rgv <- data$rgv[,N]
move <- data$move

th[1,] <- c(1,1,0,0,428)
th2[1,] <- c(2,0,0,0)

library(combinat)
x <- rep(0,3)
x[1] <- 1
y <- c(3,5,7)
temp <- permn(y)

room <- 0
for (k in 1:6) {
  # cncid <- c(1,3,5,7)
  cncid <- cbind(1,t(temp[[k]]))
  
  cncid2 <- cncid+1
  for (i in 2:4) {
    id <- cncid[i]
    id2 <- cncid2[i]
    pfor <- ceiling(th[i-1,2]/2)
    pnow <- ceiling(id/2)
    tt <- jiou(id,N)
    tl <- jiou(th[i-1,2],N)
    mt <- move[pfor,pnow]
    st <- th[i-1,3]+mt+tl
    pt <- st+cnc[(id+1)%%2+1]+tt
    at <- 0
    th[i,] <- c(i,id,st,at,pt)
    th2[i,] <- c(id2,0,0,0)
  }
  
  i <- 5
  ck <- th[(i-4):(i-1),]
  id <- ck[which.min(ck[,5]),2]
  tt <- jiou(id,N)
  pfor <- ceiling(th[i-1,2]/2)
  pnow <- ceiling(id/2)
  mt <- move[pfor,pnow]
  st <- ck[1,5]+mt
  th[i-4,4] <- st
  pt <- st+tt+cnc[(id+1)%%2+1]
  at <- 0
  th[i,] <- c(i,id,st,at,pt)
  
  id2 <- cncid2[i-4]
  st2 <- th[i-4,4]+jiou(id,N)+move[ceiling(id/2),ceiling(id2/2)]
  pt2 <- st2+31+378
  at2 <- 0
  th2[i-4,] <- c(id2,st2,at2,pt2)
  
  i <-6
  repeat{
    if(i<=8){
      ck <- th[(i-4):(i-1),]
      id <- ck[which.min(ck[,5]),2]
      # tt <- jiou(th2[i-5,2],N)
      at <- 0
      pfor <- ceiling(th2[i-5,1]/2)
      pnow <- ceiling(id/2)
      mt <- move[pfor,pnow]
      st <- th2[i-5,2]+31+mt
      pt <- st+428
      th[i-4,4] <- st
      th[i,] <- c(i,id,st,at,pt)
      id2 <- cncid2[i-4]
      st2 <- st+jiou(th[i-4,2],N)
      pt2 <- st2+31+378
      at2 <- 0
      th2[i-4,] <- c(id2,st2,at2,pt2)
      i <- i+1
    }else{
      break
    }
  }
  i <- 9
  ck <- th[(i-4):(i-1),]         #工序1滑动窗口
  leave <- which.min(ck[,5])     #找到最先发出需求的CNC
  ck.temp <- ck[leave,]          #作为临时值
  i.temp <- ck.temp[1]           #该CNC即将进行上下料作业
  ck <- ck[-leave,]
  id <- ck.temp[2]  #CNCid
  
  ck2 <- th2[(i-8):(i-5),]         #工序2滑动窗口
  leave2 <- which.min(ck2[,4])
  ck.temp2 <- ck2[leave2,]
  i.temp2 <- ck.temp2[1]
  id2 <- i.temp2
  ck2 <- ck2[-leave2,]
  
  pfor <- ceiling(th2[i-5,1]/2)  #前一位置
  pnow <- ceiling(id/2)          #现在位置
  mt <- move[pfor,pnow]          #移动所需时间
  st <- max(th2[i-5,2]+31,ck.temp[5])+mt         #上料开始时间
  pt <- st+428                   #下料开始时间
  th[i.temp,4] <- st                #CNC上次下料时间
  th[i,] <- c(i,id,st,at,pt)
  ck <- rbind(ck,th[i,])#窗口后移
  
  pfor2 <- pnow  #前一位置
  pnow2 <- ceiling(id2/2)          #现在位置
  mt2 <- move[pfor2,pnow2]    
  st2 <- max(ck.temp2[4],st+28)+mt2
  pt2 <- st2+31+378
  at2 <- 0
  th2[i-8,3] <- st2
  th2[i-4,] <- c(id2,st2,at2,pt2)
  for (i in 10:300) {
    ck <- th[(i-4):(i-1),]         #工序1滑动窗口
    leave <- which.min(ck[,5])     #找到最先发出需求的CNC
    ck.temp <- ck[leave,]          #作为临时值
    i.temp <- ck.temp[1]           #该CNC即将进行上下料作业
    ck <- ck[-leave,]
    id <- ck.temp[2]  #CNCid
    
    ck2 <- th2[(i-8):(i-5),]         #工序2滑动窗口
    leave2 <- which.min(ck2[,4])
    ck.temp2 <- ck2[leave2,]
    i.temp2 <- ck.temp2[1]
    id2 <- i.temp2
    ck2 <- ck2[-leave2,]
    
    pfor <- ceiling(th2[i-5,1]/2)  #前一位置
    pnow <- ceiling(id/2)          #现在位置
    mt <- move[pfor,pnow]          #移动所需时间
    st <- max(th2[i-5,2]+31+25,ck.temp[5])+mt         #上料开始时间
    pt <- st+428                   #下料开始时间
    th[i.temp,4] <- st                #CNC上次下料时间
    th[i,] <- c(i,id,st,at,pt)
    ck <- rbind(ck,th[i,])#窗口后移
    
    pfor2 <- pnow  #前一位置
    pnow2 <- ceiling(id2/2)          #现在位置
    mt2 <- move[pfor2,pnow2]    
    st2 <- max(ck.temp2[4],st+28)+mt2
    pt2 <- st2+31+378
    at2 <- 0
    th2[i-8,3] <- st2
    th2[i-4,] <- c(id2,st2,at2,pt2)
  }
  a <- which(th2[,3]+31+25>time.total)[1]-1
  library(xlsx)
  result <- cbind(th[1:a,1:4],th2[1:a,1:3])
  
  if(a>room){
    room <- a
    new <- result
  }
}
file <- c("data21.xls","data22.xls","data23.xls")
write.xlsx(new,file[N],
           col.names = F,row.names = F)

