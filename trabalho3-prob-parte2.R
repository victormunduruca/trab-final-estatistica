library(readxl)

populacao = read_excel("dados_trabalho.xls")

attach(populacao)
str(populacao)

valor = 0.05

dividir = split(as.numeric(grau_satisfacao), genero)

feminino = dividir$F
masculino = dividir$M

amostras<-list()
satisAmostra<-list()
generoAmostra<-list()

for (i in 1:100){
  satisAmostra[[i]] = sample(grau_satisfacao, size = 20, replace = FALSE)
  generoAmostra[[i]] = sample(genero, size = 20, replace = FALSE)
}

tteste = t.test(feminino, masculino)

tteste$p.value #Valor-P < 0,01. A hipótese nula pode ser rejeitada

testeAmostras = 0

fAmostras = 0
mAmostras = 0

##attach(amostras[1])
##vAmostras = str(amostras[1])

c20 = 0

for(i in 1:100) {
  
  d = split(as.numeric(as.character(unlist(satisAmostra[[i]]))), generoAmostra[i])
  
  fAmostras = d$F
  mAmostras = d$M
  
  testeAmostras = t.test(fAmostras, mAmostras)
  testeAmostras$p.value
  
  if(testeAmostras$p.value < valor)
    c20 = c20+1
}

##Amostras de tamanho 30

for (i in 1:100){
  satisAmostra[[i]] = sample(grau_satisfacao, size = 30, replace = FALSE)
  generoAmostra[[i]] = sample(genero, size = 30, replace = FALSE)
}

c30 = 0

for(i in 1:100) {
  
  d = split(as.numeric(as.character(unlist(satisAmostra[[i]]))), generoAmostra[i])
  
  fAmostras = d$F
  mAmostras = d$M
  
  testeAmostras = t.test(fAmostras, mAmostras)
  
  if(testeAmostras$p.value < valor)
    c30 = c30+1
}

##Amostras de tamanho 50

for (i in 1:100){
  satisAmostra[[i]] = sample(grau_satisfacao, size = 50, replace = FALSE)
  generoAmostra[[i]] = sample(genero, size = 50, replace = FALSE)
}

c50 = 0

for(i in 1:100) {
  
  d = split(as.numeric(as.character(unlist(satisAmostra[[i]]))), generoAmostra[i])
  
  fAmostras = d$F
  mAmostras = d$M
  
  testeAmostras = t.test(fAmostras, mAmostras)
  testeAmostras$p.value
  
  if(testeAmostras$p.value < valor)
    c50 = c50+1
}

##Amostras de tamanho 80

for (i in 1:100){
  satisAmostra[[i]] = sample(grau_satisfacao, size = 80, replace = FALSE)
  generoAmostra[[i]] = sample(genero, size = 80, replace = FALSE)
}

c80 = 0

for(i in 1:100) {
  
  d = split(as.numeric(as.character(unlist(satisAmostra[[i]]))), generoAmostra[i])
  
  fAmostras = d$F
  mAmostras = d$M
  
  testeAmostras = t.test(fAmostras, mAmostras)
  testeAmostras$p.value
  
  if(testeAmostras$p.value < valor)
    c80 = c80+1
}

##Amostras de tamanho 120

for (i in 1:100){
  satisAmostra[[i]] = sample(grau_satisfacao, size = 120, replace = FALSE)
  generoAmostra[[i]] = sample(genero, size = 120, replace = FALSE)
}

c120 = 0

for(i in 1:100) {
  
  d = split(as.numeric(as.character(unlist(satisAmostra[[i]]))), generoAmostra[i])
  
  fAmostras = d$F
  mAmostras = d$M
  
  testeAmostras = t.test(fAmostras, mAmostras)
  testeAmostras$p.value
  
  if(testeAmostras$p.value < valor)
    c120 = c120+1
}

##Amostras de tamanho 150

for (i in 1:100){
  satisAmostra[[i]] = sample(grau_satisfacao, size = 150, replace = FALSE)
  generoAmostra[[i]] = sample(genero, size = 150, replace = FALSE)
}

c150 = 0

for(i in 1:100) {
  d = split(as.numeric(as.character(unlist(satisAmostra[[i]]))), generoAmostra[i])
  
  fAmostras = d$F
  mAmostras = d$M
  
  fAmostras
  
  testeAmostras = t.test(fAmostras, mAmostras)
  testeAmostras$p.value
  
  if(testeAmostras$p.value < valor)
    c150 = c150+1
}
