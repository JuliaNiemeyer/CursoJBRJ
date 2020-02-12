##script pra ler tabela csv do exercicio
tabela <- read.csv("./data/ex_Julia.csv", header = T, sep = ';')
head(tabela)


library(tidyverse)
# ps
# -- Conflicts ------------------------------------------ tidyverse_conflicts() --
# x dplyr::filter() masks stats::filter()
# x dplyr::lag()    masks stats::lag()
## isso quer dizer que tem funções iguais pra dois pacotes. Então tem que usar esses dois pontinhos
# explicitando de qual pacote quero usar a função -> dplyr::filter() mascara a função stats::filter()
