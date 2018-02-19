#install.packages("readxl")
library("readxl")

dados <- read_excel("dados_trabalho.xls")


dados$grau_dependencia
attach(dados)
str(dados)



tab_grau = as.numeric(grau_dependencia)
mean(tab_grau) 
media_geral = mean(tab_grau)
contadores = c(0,0,0,0,0,0)
i = 1
cgeral = 0
for(tamanho_amostra in c(20, 30, 50, 400, 500, 600)) {
  for(j in 1:800) {
    amostra = as.numeric(sample(grau_dependencia, tamanho_amostra, replace = FALSE, prob = NULL))
    media_amostra = mean(amostra)
    esquerda = t.test(amostra)$conf[1]
    direita = t.test(amostra)$conf[2]
    if(media_geral > esquerda && media_geral < direita) {
      contadores[i] = contadores[i]+1
      cgeral = cgeral+1
    }
  }
  i = i + 1
}
contadores/800


