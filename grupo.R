SkewNormal <- function(n, sigma2, lambda){
  delta <- lambda / sqrt(1 + lambda^2)
  deltag<-sqrt(sigma2)*delta
  mu<--sqrt(2/pi)*deltag
  y <- mu*rep(1,n) + sqrt(sigma2)*(delta*abs(rnorm(n)) +(1 - delta^2)^(1/2)*rnorm(n))
  return(y)}

set.seed(2016)
n<-100
x=NULL
e=NULL
y=NULL
a<--2
b<-2
sigma2<-2
lambda<--3
beta0<-1
beta1<--5


for(i in 1:n){
  x[i]<- runif(1,a,b)
  e[i]<-SkewNormal(1, sigma2, lambda)
  y[i]<-beta0+beta1*x[i]+e[i]
}

hist(y)

#Contaminar para 10
v=NULL
v[1]=10
for(i in 2:15){
  v[i]=v[i-1]+10
}

d = sample(1:100, 3, replace=FALSE, prob=NULL) #escolher aleatoriamente as amostras contaminadas

c=NULL
for(i in 1:15){
  c=c(c,(1+(v[i]/100))*y[d])
  
}
c = matrix(data = c, nrow = 15, ncol = 3) #cada linha da matriz é pra um valor de v

ynovo=NULL
ynovo<-matrix(rep(y, 15), nrow=15, ncol=100, byrow=TRUE)

for(i in 1:15)
  {
      ynovo[i,d[1]]=c[i,1]
    
  }
for(i in 1:15)
{
  ynovo[i,d[2]]=c[i,2]
  
}
for(i in 1:15)
{
  ynovo[i,d[3]]=c[i,3]
  
}


library(nlsmsn)


analysis.sn<-smsn.nl(y=y,x=x,betas=c(20,40,1), 
                     sigma2=1,shape =1, nlf = nlf, 
                     criteria = TRUE,family = "Skew.normal",iter.max = 300)









