swap <- function(x){
  temp <- x[1,]
  x[1,] <- x[2,]
  x[2,] <- temp
  return(x)
}

