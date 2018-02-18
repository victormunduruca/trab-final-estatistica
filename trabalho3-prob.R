install.packages("readxl")
library("readxl")

dados <- read_excel("dados_trabalho.xls")


dados$grau_dependencia
attach(dados)
str(dados)




tab_grau = as.numeric(grau_dependencia)
mean(tab_grau) # da erro ???

amostra = sample(grau_dependencia, 2, replace = FALSE, prob = NULL)
mean(amostra)


