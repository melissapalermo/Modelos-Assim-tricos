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
########################SKEW NORMAL########################################################


Lambda = function(i){
  vetaic=NULL
  vetbic=NULL
  vetbeta1=NULL
  vetbeta2=NULL
  
  vetaicy=NULL
  vetbicy=NULL
  vetbeta1y=NULL
  vetbeta2y=NULL
  
  rcb1=NULL
  rcb2=NULL
  for(j in 1:100){
  
  e<-SkewNormal(100, 0.5, 1.5)
  y= betas[1]+betas[2]*x+e
  d=sample(1:100, 3, replace=FALSE, prob=NULL)
  c=(1+(v[i]/100))*y[d]
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
  
  rcb1=c(rcb1, abs((fity$betas[1]-fit$betas[1])/fity$betas[1])*100)
  rcb2=c(rcb2, abs((fity$betas[2]-fit$betas[2])/fity$betas[2])*100)
  
  
  }
  lambda =c(mean(vetaicy), mean(vetbicy), mean(vetaic), mean(vetbic), mean(rcb2), mean(rcb1))
  
  return(lambda)
}

Lambda(1)
Lambda(2)
Lambda(3)
Lambda(4)
Lambda(5)
Lambda(6)
Lambda(7)
Lambda(8)
Lambda(9)
Lambda(10)
Lambda(11)
Lambda(12)
Lambda(13)
Lambda(14)
Lambda(15)
######################################################################################

####################SKEW T#################################################

vetaic=NULL
vetbic=NULL
vetbeta1=NULL
vetbeta2=NULL

vetaicy=NULL
vetbicy=NULL
vetbeta1y=NULL
vetbeta2y=NULL

rcb1=NULL
rcb2=NULL

for(j in 1:100){
  
  e<-SkewNormal(100, 0.5, 1.5)
  y= betas[1]+betas[2]*x+e
  d=sample(1:100, 3, replace=FALSE, prob=NULL)
  c=(1+(v[1]/100))*y[d]
  ynovo=y
  ynovo[d]=c
  fit=smsn.nl(y=ynovo,x=x,betas=c(1.5,2.5), 
              sigma2=.25,shape =.75, nlf = function(x, betas){betas[1]+betas[2]*x}, 
              criteria = TRUE,family = "Skew.t",iter.max = 300, nu=2.6)
  fity=smsn.nl(y=y,x=x,betas=c(1.5,2.5), 
               sigma2=.25,shape =.75, nlf = function(x, betas){betas[1]+betas[2]*x}, 
               criteria = TRUE,family = "Skew.t",iter.max = 300, nu=2.6)
  vetaic=c(vetaic,fit$AIC )
  vetbic=c(vetbic,fit$BIC )
  vetbeta1=c(vetbeta1,fit$betas[1] )
  vetbeta2=c(vetbeta2,fit$betas[2] )
  
  vetaicy=c(vetaicy,fity$AIC )
  vetbicy=c(vetbicy,fity$BIC )
  vetbeta1y=c(vetbeta1y,fity$betas[1] )
  vetbeta2y=c(vetbeta2y,fity$betas[2] )
  
  rcb1=c(rcb1, abs((fity$betas[1]-fit$betas[1])/fity$betas[1])*100)
  rcb2=c(rcb2, abs((fity$betas[2]-fit$betas[2])/fity$betas[2])*100)
  
  
}

lambda10 =c(mean(vetaic), mean(vetbic), mean(vetaicy), mean(vetbicy), mean(rcb2), mean(rcb1))




