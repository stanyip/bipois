dbipois <- function(x,y,lambda1,lambda2,lambda3=0,log=FALSE) {
  dat = rbind(x,y)
  dat = split(dat, rep(1:ncol(dat), each = nrow(dat)))
  u = exp(-(lambda1+lambda2+lambda3))*lambda1^x*lambda2^y/factorial(x)/factorial(y)
  v = u * sapply(dat, function(z) {u1=min(z); sum(sapply(0:u1,function(k) choose(z[1],k)*choose(z[2],k)*factorial(k)*(lambda3/lambda1/lambda2)^k))})
  if (log) return(log(v))
  v
}
