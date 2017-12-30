dbipois <- function(x,y,lambda1,lambda2,lambda3=0,log=FALSE) {
  dat = rbind(x,y)
  dat = split(dat, rep(1:ncol(dat), each = nrow(dat)))
  if (!lambda3) if (!log) return(dpois(x,lambda1) * dpois(y,lambda2)) else return(dpois(x,lambda1, log=T) + dpois(y,lambda2, log=T))
  u = (-(lambda1+lambda2+lambda3))+x*log(lambda1)+y*log(lambda2) - lfactorial(x) - lfactorial(y)
  v = sapply(dat, function(z) {u1=min(z); sum(sapply(0:u1,function(k) exp(lchoose(z[1],k)+lchoose(z[2],k)+lfactorial(k)+k*(log(lambda3)-log(lambda1)-log(lambda2)))))})
  v = ifelse(v==Inf,0,v)
  if (log) return(u*log(v))
  exp(u) * v
}
