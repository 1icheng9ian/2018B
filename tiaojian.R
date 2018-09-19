tiaojian <- function(N){
  if(N==1){
    # 第一组
    time.move <- matrix(c(0,20,33,46,20,0,20,33,33,20,0,20,46,33,20,0),nrow = 4)
    th[1,] <- c(1,startp,0,588,588)
    tt[1] <- 28
  }else if(N==2){
    # 第二组
    time.move <- matrix(c(0,23,41,59,23,0,23,41,41,23,0,23,59,41,23,0),nrow = 4)
    th[1,] <- c(1,startp,0,610,610)
    tt[1] <- 30
  }else if(N==3){
    # 第三组
    time.move <- matrix(c(0,18,32,46,18,0,18,32,32,18,0,18,46,32,18,0),nrow = 4)
    th[1,] <- c(1,startp,0,572,572)
    tt[1] <- 27
  }
  time.cnc <- matrix(c(560,400,378,580,280,500,545,455,182),nrow = 3)
  time.rgv <- matrix(c(28,31,30,35,27,32),nrow = 2)
  time.wash <- matrix(c(25,30,25),nrow = 1)
  
  colnames(th) <- c('物料编号','CNC编号','上料开始时间',
                    '下料开始时间','发出下料指令时间')
  result <- list(cnc=time.cnc,rgv=time.rgv,
                 wash=time.wash,th=th,time=tt,
                 move=time.move)
  return(result)
}