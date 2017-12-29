rbipois <- function(n,lambda1,lambda2,lambda3=0) {
  rnd = log(runif(n))
  m = 2
  u = log(sum(sapply(0:m,function(x) sum(dbipois(0:x,rev(0:x),lambda1,lambda2,lambda3)))))
  while (u < max(rnd)) {
    m = m*2
    u = log(sum(sapply(0:m,function(x) sum(dbipois(0:x,rev(0:x),lambda1,lambda2,lambda3)))))
  }
  dat = Reduce('rbind',lapply(0:m,function(x) cbind(0:x,rev(0:x))))
  dat[findInterval(rnd,log(cumsum(dbipois(dat[,1],dat[,2],lambda1,lambda2,lambda3))))+1,]
}
