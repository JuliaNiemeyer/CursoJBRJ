####Aula 7. Criando e salvando gráficos no R

##Explorando o dataset
#Vamos extrair a riqueza de espécies por Site e plotar a riqueza de espécies em função do teor de argila (clay), silte (silt) e areia (sand) do solo. (aula de ontem)

# lendo os dados
comm <- read.csv("data/cestes/comm.csv")
envir <- read.csv("data/cestes/envir.csv")
# explore os dados com as funções head e summary

#Para extrair a riqueza por Site, lembremos da aula de ontem:

comm.pa <- comm[, -1] > 0 #pega apenas presença e ignora a primeira coluna que são os sites
rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum) #soma por linhas a quantidade de TRUE (presença) (todas as sps juntas num mesmo site no caso)
head(rich)

summary(rich)

#Os dados que vêm no summary do objeto rich também podem ser observados realizando um boxplot:

boxplot(rich)
boxplot(rich, las = 1)

##Criando uma nova tabela com a coluna de riqueza
#Vamos acrescentar a coluna de riqueza à tabela de variáveis ambientais num objeto novo que se chame localidades

localidades <- cbind(envir, rich)
head(localidades)


##Gráfico de dispersão
#plot com abline do modelo de regressão

# criando modelos lineares
riqsilt <- lm(rich ~ Silt, data = localidades)
riqclay <- lm(rich ~ Clay, data = localidades)
riqsand <- lm(rich ~ Sand, data = localidades)

# extraindo os coeficientes do modelo
coef_s <- coef(riqsilt)
coef_c <- coef(riqclay)
coef_d <- coef(riqsand)

# definindo os limites dos eixos
limy <- c(min(localidades$rich),
          max(localidades$rich))
limx <- c(min(localidades[,c("Clay", "Sand", "Silt")]),
          max(localidades[,c("Clay", "Sand", "Silt")]))

## definindo o nome do eixo y
laby <- "Riqueza de espécies"

#Agora vamos construir o gráfico em si. Procure o significado dos parâmetros mfrow e bty na função par()

# define parametros graficos

par(mfrow = c(1, 3),
    las = 1,
    bty = "l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas

# plot da riqueza em função do teor de Silte
plot(rich ~ Silt, data = localidades,
     col = "tomato",
     ylim = limy, xlim = limx,
     ylab = laby,
     xlab = "Teor de Silte (%)")
# linha do previsto pelo modelo
## a + b*x
abline(a = coef_s[1], b = coef_s[2],
       col = 'tomato', lwd = 2)
mtext("A", 3, adj = 0, font = 2)

## plot da riqueza em função do teor de Argila
plot(rich ~ Clay, data = localidades,
     col = "navy",
     ylim = limy, xlim = limx,
     ylab = "",
     xlab = "Teor de Argila (%)")
mtext("B", 3, adj = 0, font = 2)
# linha do previsto pelo modelo
## a + b*x
abline(a = coef_c[1],
       b = coef_c[2],
       col = 'navy',
       lwd = 2)

## plot da riqueza em função do teor de Areia
plot(rich ~ Sand, data = localidades,
     col = "dodgerblue",
     ylim = limy, xlim = limx,
     ylab = "",
     xlab = "Teor de Areia (%)")
mtext("C", 3, adj = 0, font = 2)
# linha do previsto pelo modelo
## a + b*x
abline(a = coef_d[1],
       b = coef_d[2],
       col = 'dodgerblue',
       lwd = 2)
#Explore outros parâmetros da função par(), como pch e cex.

#Exportando o gráfico com as funções png() e dev.off()
#Queremos exportar esse gráfico. Para isso, vamos salvar no diretório /figs em seu repositório. Para exportar o gráfico vamos usar a função png(), especificando a resolução e dimensões da figura. Quando criamos gráficos com a função png() o que fazemos é:

#1. anunciar qual extensão e arquivo vamos plotar o gráfico com a função png()

#2. determinar a sequência de comandos que cria o gráfico

#3. finalizar a construção do arquivo com a função dev.off()
