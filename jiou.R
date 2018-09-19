jiou <- function(x,N){
  time.rgv <- matrix(c(28,31,30,35,27,32),nrow = 2)
  if(x%%2==1){
    tt <- time.rgv[1,N]
  }else{
    tt <- time.rgv[2,N]
  }
  return(tt)
}