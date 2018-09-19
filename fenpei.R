fenpei <- function(th){
  L <- 8
  ck <- matrix(nrow = L,ncol = 5)
  ck <- th[1:8,]
  source("Sliding_window.R")
  i <- 9
  repeat{
    ck <- Sliding_window(i,ck)
    th[i,] <- ck[8,]
    fin.time <- th[i,4]+jiou(th[i,2],N)+time.wash[1,N]
    if(fin.time>=time.total){
      break
    }else
      i <- i+1
  }
  number <- max(th[,1])
  
  result <- list(num = number,fn = th)
  return(result)
}