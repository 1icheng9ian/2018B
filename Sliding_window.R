Sliding_window <- function(i,ck){
  leave <- which.min(ck[,5])
  ck.temp <- ck[leave,]
  ck <- ck[-leave,]
  id <- ck.temp[2]
  ttt <- jiou(id,N)
  ttl <- jiou(ck[which.max(ck[,2]),2],N)
  st <- ck.temp[4]
  pt <- st+time.cnc[1,N]+ttt
  pnow <- ceiling(id/2)
  pforward <- ceiling(ck[which.max(ck[,4]),2]/2)
  mt <- time.move[pnow,pforward]
  maxt <- max(pt,ttl+time.wash[1,N]+ck[which.max(ck[,4]),])
  at <- mt+maxt
  th[i,] <- c(i,id,st,at,pt)
  ck <- rbind(ck,th[i,])
  return(ck)
}