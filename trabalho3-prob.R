install.packages("readxl")
library("readxl")

dados <- read_excel("dados_trabalho.xls")



attach(dados)
str(dados)

grau_dependencia

tab_grau = table(grau_dependencia)
median(tab_grau)
