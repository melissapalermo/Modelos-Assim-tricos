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

bic_c=c(278.0256505, 317.0797231, 357.5580828, 407.7363787, 443.0029863, 476.1105814, 500.6382423,
        519.5405116, 550.8966216 , 569.9712436, 579.8890336, 604.6527978, 614.5369833, 631.036868, 644.66607)
######################################################################################

####################SKEW T#################################################

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
  lambda =c(mean(vetaicy), mean(vetbicy), mean(vetaic), mean(vetbic), mean(rcb2), mean(rcb1))
  
  return(lambda)
}

vet1=Lambda(1)
vet2=Lambda(2)
vet3=Lambda(3)
vet4=Lambda(4)
vet5=Lambda(5)
vet6=Lambda(6)
vet7=Lambda(7)
vet8=Lambda(8)
vet9=Lambda(9)
vet10=Lambda(10)
vet11=Lambda(11)
vet12=Lambda(12)
vet13=Lambda(13)
vet14=Lambda(14)
vet15=Lambda(15)

aic=c(vet1[1], vet2[1], vet3[1], vet4[1], vet5[1], vet6[1], vet7[1], vet8[1]
      , vet9[1], vet10[1], vet11[1], vet12[1], vet13[1], vet14[1], vet15[1])

bic=c(vet1[2], vet2[2], vet3[2], vet4[2], vet5[2], vet6[2], vet7[2], vet8[2]
      , vet9[2], vet10[2], vet11[2], vet12[2], vet13[2], vet14[2], vet15[2])

aic_c=c(vet1[3], vet2[3], vet3[3], vet4[3], vet5[3], vet6[3], vet7[3], vet8[3]
      , vet9[3], vet10[3], vet11[3], vet12[3], vet13[3], vet14[3], vet15[3])

bic_c=c(vet1[4], vet2[4], vet3[4], vet4[4], vet5[4], vet6[4], vet7[4], vet8[4]
      , vet9[4], vet10[4], vet11[4], vet12[4], vet13[4], vet14[4], vet15[4])

RC_b1=c(vet1[5], vet2[5], vet3[5], vet4[5], vet5[5], vet6[5], vet7[5], vet8[5]
            , vet9[5], vet10[5], vet11[5], vet12[5], vet13[5], vet14[5], vet15[5])

RC_b2=c(vet1[6], vet2[6], vet3[6], vet4[6], vet5[6], vet6[6], vet7[6], vet8[6]
        , vet9[6], vet10[6], vet11[6], vet12[6], vet13[6], vet14[6], vet15[6])

plot(v,aic, ylab="AIC sem contaminação", xlab="Lambda", main="AIC's Skew t")
plot(v,Bic, ylab="BIC sem contaminação", xlab="Lambda", main="BIC's Skew t")
plot(v,aic_c, ylab="AIC com contaminação", xlab="Lambda", main="AIC's Skew t")
plot(v,bic_b, ylab="BIC com contaminação", xlab="Lambda", main="BIC's Skew t")

###############################################################################
########################### SSL######################################

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
    fit=smsn.nl(y=ynovo,x=x,betas=c(5,5), 
                sigma2=.25,shape =.75, nlf = function(x, betas){betas[1]+betas[2]*x}, 
                criteria = TRUE,family = "Skew.slash",iter.max = 300, nu=.4)
    fity=smsn.nl(y=y,x=x,betas=c(5,5), 
                 sigma2=.25,shape =.75, nlf = function(x, betas){betas[1]+betas[2]*x}, 
                 criteria = TRUE,family = "Skew.slash",iter.max = 300, nu=.4)
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

vet1=Lambda(1)
vet2=Lambda(2)
vet3=Lambda(3)
vet4=Lambda(4)
vet5=Lambda(5)
vet6=Lambda(6)
vet7=Lambda(7)
vet8=Lambda(8)
vet9=Lambda(9)
vet10=Lambda(10)
vet11=Lambda(11)
vet12=Lambda(12)
vet13=Lambda(13)
vet14=Lambda(14)
vet15=Lambda(15)

aic=c(vet1[1], vet2[1], vet3[1], vet4[1], vet5[1], vet6[1], vet7[1], vet8[1]
      , vet9[1], vet10[1], vet11[1], vet12[1], vet13[1], vet14[1], vet15[1])

bic=c(vet1[2], vet2[2], vet3[2], vet4[2], vet5[2], vet6[2], vet7[2], vet8[2]
      , vet9[2], vet10[2], vet11[2], vet12[2], vet13[2], vet14[2], vet15[2])

aic_c=c(vet1[3], vet2[3], vet3[3], vet4[3], vet5[3], vet6[3], vet7[3], vet8[3]
        , vet9[3], vet10[3], vet11[3], vet12[3], vet13[3], vet14[3], vet15[3])

bic_c=c(vet1[4], vet2[4], vet3[4], vet4[4], vet5[4], vet6[4], vet7[4], vet8[4]
        , vet9[4], vet10[4], vet11[4], vet12[4], vet13[4], vet14[4], vet15[4])

RC_b1=c(vet1[5], vet2[5], vet3[5], vet4[5], vet5[5], vet6[5], vet7[5], vet8[5]
        , vet9[5], vet10[5], vet11[5], vet12[5], vet13[5], vet14[5], vet15[5])

RC_b2=c(vet1[6], vet2[6], vet3[6], vet4[6], vet5[6], vet6[6], vet7[6], vet8[6]
        , vet9[6], vet10[6], vet11[6], vet12[6], vet13[6], vet14[6], vet15[6])

plot(v,aic, ylab="AIC sem contaminação", xlab="Lambda", main="AIC's Skew Slash")
plot(v,bic, ylab="BIC sem contaminação", xlab="Lambda", main="BIC's Skew Slash")
plot(v,aic_c, ylab="AIC com contaminação", xlab="Lambda", main="AIC's Skew Slash")
plot(v,bic_c, ylab="BIC com contaminação", xlab="Lambda", main="BIC's Skew Slash")

################################################################################
#######################SSkew CN########################################

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
    fit=smsn.nl(y=ynovo,x=x,betas=c(6,6), 
                sigma2=.25,shape =.75, nlf = function(x, betas){betas[1]+betas[2]*x}, 
                criteria = TRUE,family = "Skew.cn",iter.max = 300, nu=c(0.2,0.1))
    fity=smsn.nl(y=y,x=x,betas=c(1.5,2.5), 
                 sigma2=.25,shape =.75, nlf = function(x, betas){betas[1]+betas[2]*x}, 
                 criteria = TRUE,family = "Skew.cn",iter.max = 300, nu=c(0.2,0.1))
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

vet1=Lambda(1)
vet2=Lambda(2)
vet3=Lambda(3)
vet4=Lambda(4)
vet5=Lambda(5)
vet6=Lambda(6)
vet7=Lambda(7)
vet8=Lambda(8)
vet9=Lambda(9)
vet10=Lambda(10)
vet11=Lambda(11)
vet12=Lambda(12)
vet13=Lambda(13)
vet14=Lambda(14)
vet15=Lambda(15)

aic=c(vet1[1], vet2[1], vet3[1], vet4[1], vet5[1], vet6[1])

bic=c(vet1[2], vet2[2], vet3[2], vet4[2], vet5[2], vet6[2])

aic_c=c(vet1[3], vet2[3], vet3[3], vet4[3], vet5[3], vet6[3])

bic_c=c(vet1[4], vet2[4], vet3[4], vet4[4], vet5[4], vet6[4])

RC_b1=c(vet1[5], vet2[5], vet3[5], vet4[5], vet5[5], vet6[5])

RC_b2=c(vet1[6], vet2[6], vet3[6], vet4[6], vet5[6], vet6[6])

v=c(10, 20, 30, 40, 50, 60)
plot(v,aic, ylab="AIC sem contaminação", xlab="Lambda", main="AIC's Skew CN")
plot(v,bic, ylab="BIC sem contaminação", xlab="Lambda", main="BIC's Skew CN")
plot(v,aic_c, ylab="AIC com contaminação", xlab="Lambda", main="AIC's Skew CN")
plot(v,bic_c, ylab="BIC com contaminação", xlab="Lambda", main="BIC's Skew CN")

