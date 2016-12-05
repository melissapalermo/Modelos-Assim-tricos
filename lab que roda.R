set.seed(2)
betas=c(10,8) #fixo

x<- runif(100,0,7) #fixo

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

RC_b1=c(0.4088348, 0.3097706, 0.4763819, 0.4407507, 0.5398594, 0.5842518, 0.5822508, 
        0.7109691, 0.6836415, 0.7612953, 0.7267882, 0.8284495, 0.8982624,
        1.037166, 1.249638)

RC_b2=c(0.8402188, 5.9112512, 9.5088393, 14.8244151, 19.4752950, 24.1192783, 29.2088891, 
        33.0118248, 39.5399000, 44.1213163, 47.3327494, 54.0597794,57.6841737,
        63.003166,  68.016112)

aic=c(159.3629805,161.5744986, 161.7615820, 161.7615442, 162.1942392,  162.7343480,
      160.3315182, 160.3777125, 160.3769370, 162.3727503, 159.8336417, 159.1400877, 
      162.0547393, 158.304089, 160.909557 )

bic=c(169.7836613, 171.9951793, 172.1822628, 172.1822249, 172.6149199, 173.1550287, 170.7521989,
      170.7983932, 170.7976178, 172.7934311, 170.2543224, 169.5607684, 172.4754201,168.724770,171.330238)

aic_c=c(267.6049698, 306.6590424, 347.1374020, 397.3156979, 432.5823056, 465.6899006, 490.2175616,
        509.1198308, 540.4759409, 559.5505628, 569.4683529, 594.2321170, 604.1163026, 620.616187, 634.245391 )
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




