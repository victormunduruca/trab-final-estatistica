#install.packages("readxl")
library("readxl")

dados <- read_excel("dados_trabalho.xls")


dados$grau_dependencia
attach(dados)
str(dados)



tab_grau = as.numeric(grau_dependencia)
mean(tab_grau) 
contadores = c(0,0,0,0,0,0)
i = 1
for(tamanho_amostra in c(20, 30, 50, 80, 120, 150)) {
  for(j in 1:100) {
    amostra = as.numeric(sample(grau_dependencia, tamanho_amostra, replace = FALSE, prob = NULL))
    media_amostra = mean(amostra)
    esquerda = t.test(amostra)$conf[1]
    direita = t.test(amostra)$conf[2]
    if(media_amostra > esquerda && media_amostra < direita) {
      contadores[i] = contadores[i]+1
    }
  }
  i = i + 1
}




