install.packages("readxl")
library("readxl")

dados <- read_excel("dados_trabalho.xls")


dados$grau_dependencia
attach(dados)
str(dados)




tab_grau = table(grau_dependencia)
mean(grau_dependencia) # da erro ???

amostra = sample(grau_dependencia, 2, replace = FALSE, prob = NULL)
mean(amostra)


