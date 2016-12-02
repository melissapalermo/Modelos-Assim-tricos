set.seed(2)
betas=c(2,5) #fixo

x<- runif(100,-7,7) #fixo

SkewNormal <- function(n, sigma2, lambda){ #gera o ei (erro skwe normal)
  delta <- lambda / sqrt(1 + lambda^2)
  deltag<-sqrt(sigma2)*delta
  mu<--sqrt(2/pi)*deltag
  y <- mu*rep(1,n) + sqrt(sigma2)*(delta*abs(rnorm(n)) +(1 - delta^2)^(1/2)*rnorm(n))
  return(y)}

v=NULL #lambidas
v[1]=10
for(i in 2:15){
  v[i]=v[i-1]+10
}

vetaic=NULL
vetbic=NULL
vetbeta1=NULL
vetbeta2=NULL

vetaicy=NULL
vetbicy=NULL
vetbeta1y=NULL
vetbeta2y=NULL

for(j in 1:100){
  
  e<-SkewNormal(100, 0.5, 1.5)
  y= betas[1]+betas[2]*x+e
  d=sample(1:100, 3, replace=FALSE, prob=NULL)
  c=(1+(v[1]/100))*y[d]
  ynovo=y
  ynovo[d]=c
  fit=smsn.nl(y=ynovo,x=x,betas=c(1.5,2.5), 
              sigma2=.25,shape =.75, nlf = function(x, betas){betas[1]+betas[2]*x}, 
              criteria = TRUE,family = "Skew.normal",iter.max = 300)
  fity=smsn.nl(y=y,x=x,betas=c(1.5,2.5), 
              sigma2=.25,shape =.75, nlf = function(x, betas){betas[1]+betas[2]*x}, 
              criteria = TRUE,family = "Skew.normal",iter.max = 300)
  vetaic=c(vetaic,fit$AIC )
  vetbic=c(vetbic,fit$BIC )
  vetbeta1=c(vetbeta1,fit$betas[1] )
  vetbeta2=c(vetbeta2,fit$betas[2] )
  
  vetaicy=c(vetaicy,fity$AIC )
  vetbicy=c(vetbicy,fity$BIC )
  vetbeta1y=c(vetbeta1y,fity$betas[1] )
  vetbeta2y=c(vetbeta2y,fity$betas[2] )
  
  
  
}

maic=mean(vetaic)


betas=c(2,5)

x<- runif(100,-7,7)

SkewNormal <- function(n, sigma2, lambda){
  delta <- lambda / sqrt(1 + lambda^2)
  deltag<-sqrt(sigma2)*delta
  mu<--sqrt(2/pi)*deltag
  y <- mu*rep(1,n) + sqrt(sigma2)*(delta*abs(rnorm(n)) +(1 - delta^2)^(1/2)*rnorm(n))
  return(y)}

v=NULL
v[1]=10
for(i in 2:15){
  v[i]=v[i-1]+10
}

vetaic=NULL
vetbic=NULL
vetbetas=NULL

for(j in 1:100){
  
  e<-SkewNormal(100, 0.5, 1.5)
  y= betas[1]+betas[2]*x+e
  d=sample(1:100, 3, replace=FALSE, prob=NULL)
  c=(1+(v[1]/100))*y[d]
  ynovo=y
  ynovo[d]=c
  fit=smsn.nl(y=ynovo,x=x,betas=c(1.5,2.5), 
              sigma2=.25,shape =.75, nlf = function(x, betas){betas[1]+betas[2]*x}, 
              criteria = TRUE,family = "Skew.t",iter.max = 300, nu=.35)
  vetaic=c(vetaic,fit$AIC )
  vetbic=c(vetbic,fit$BIC )
  vetbetas=c(vetbetas,fit$betas )
  
}

maic=mean(vetaic)

>>>>>>> origin/master
