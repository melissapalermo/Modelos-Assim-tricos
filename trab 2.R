#enunciado https://www.dropbox.com/s/od9cca5oo9q4o0b/Trabalho%202.pdf?dl=0

library(sn)
#1 a) gerar 100 uniforme [a,b]. a=1, b=0

x=runif(100, -2,2)

#1b) v = 10, 20, ..., 150
v=NULL
v[1]=10
for(i in 2:15){
  v[i]=v[i-1]+10
}

#1c) j=1,...,100

j=rep(1:100)

#1d) amostra aleatória

n = 100
beta0 = -1
beta1 = -4
beta2 = -3
nu = 3
sigma2 = 2
lambda = 2
  
y=NULL
e=NULL
 
for(i in 1:n){
  e[i] = rsn(1,-sqrt(2/pi)*nu,sigma2,lambda)
  y[i] = beta0 + beta1*x[i] + e[i]
}