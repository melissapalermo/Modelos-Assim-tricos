#enunciado https://www.dropbox.com/s/od9cca5oo9q4o0b/Trabalho%202.pdf?dl=0

#1 a) gerar 100 uniforme [a,b]. a=1, b=0

x=runif(100, 0, 1)

#1b) v = 10, 20, ..., 150
v=NULL
v[1]=10
for(i in 2:15){
  v[i]=v[i-1]+10
}
