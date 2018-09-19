chushi <- function(i,cnc.id){
  
  p <- ceiling(cnc.id/2)
  
  id <- cnc.id[i]
  tt <- jiou(id,N)
  ttl <- jiou(cnc.id[i-1],N)
  mt <- time.move[p[i],p[i-1]]
  st <- th[i-1,3]+mt+ttl
  pt <- st+tt+time.cnc[1,N]
  at <- max(pt,(th[i-1,4]+ttl))+mt
  th[i,] <- c(i,id,st,at,pt)
  return(th)
}