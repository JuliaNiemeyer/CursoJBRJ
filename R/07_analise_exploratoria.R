## Tutorial de Análise Exploratória de Dados

# Parte 1: O quarteto de Anscombe
data("anscombe")

dim(anscombe) # dimensao dos dados, N de linhas e N de colunas
head(anscombe) # seis primeiras linhas dos dados
class(anscombe) # classe do objeto
str(anscombe) # estrutura do objeto

# Vamos fazer a média por das colunas com x.
mean(anscombe$x1)
mean(anscombe$x2)
mean(anscombe$x3)
mean(anscombe$x4)

# o mesmo calculo, agora apenas em 1 linha de comando
## media de todos os vetores x
apply(anscombe[,1:4], 2, mean) #aplica uma funcao a todas as linhas de um objeto
## media de todos os vetores y
apply(anscombe[,5:8], 2, mean)

# variância dos dados
apply(anscombe, 2, var) # aplica a funcao var a todas as linhas do objeto


# correlação
cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)
# coeficiente de regressão
## primeiro criamos objetos com as regressoes dos quatro conjuntos
m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)
## vamos criar agora uma lista com todos os modelos para facilitar o trabalho
mlist <- list(m1, m2, m3, m4)
## agora sim podemos calcular de forma menos repetitiva os coeficientes de regressao
lapply(mlist, coef)

anscombe

# funcao par para definir as configuracoes da janela grafica entre em ?par
par(mfrow = c(2, 2), #abre uma janela gráfica com 2 linhas  e 2 colunas
    las = 1, # deixa as legendas dos eixos na vertical
    bty = "l") # tipo do box do grafico em L

plot(anscombe$y1 ~ anscombe$x1) #plot das variaveis
abline(mlist[[1]]) # adicionando a reta prevista pelo modelo de regressao
plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])
plot(anscombe$y3 ~ anscombe$x3)
abline(mlist[[3]])
plot(anscombe$y4 ~ anscombe$x4)
abline(mlist[[4]])

par(mfrow = c(1, 1)) # retorna a janela grafica para o padrao de 1 linha e 1 coluna


#######################
###################
##########
## Parte 2: Uma rotina (entre muitas possíveis) de análise exploratória

data("iris")
?iris

head(iris)
summary(iris)

###Conhecendo as funções aggregate e tapply
# As duas funções são semelhantes, o que muda são os argumentos e o formato de saída de cada uma delas.

table(iris$Species)

# media do comprimento de sepala por especie
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)
# a mesma tarefa, executada por outra funcao. Outros argumentos e outra saída
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)
# ainda a mesma tarefa, com a mesma função mas em uma notação diferente
aggregate(Sepal.Length ~ Species, data = iris, mean)

#Podemos fazer o mesmo para as outras variáveis.

aggregate(Sepal.Width ~ Species, data = iris, mean)
aggregate(Petal.Length ~ Species, data = iris, mean)
aggregate(Petal.Width ~ Species, data = iris, mean)

##E agora vamos calcular o desvio padrão das variáveis

tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
##mesma coisa com aggregate
aggregate(Sepal.Length ~ Species, data = iris, sd)

tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
##mesma coisa com aggregate
aggregate(Sepal.Width ~ Species, data = iris, sd)

tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)


##Veja abaixo uma solução de como calular a média por espécie de todas as variáveis. Para isso, vamos usar o comando for e executar todas as tarefas em um mesmo ciclo.

# criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica
medias <- matrix(NA, ncol = 3, nrow = 4)
# definindo o nome das colunas e das linhas da matriz
colnames(medias) <- unique(iris$Species)
rownames(medias) <- names(iris)[-5]

for (i in 1:4) {
  medias[i,] <- tapply(iris[,i], iris$Species, mean)
}


##################################
#######################
########## Estatísticas descritivas
#### Medidas de tendência central

#Média

##criar objeto vars que retira a coluna de espécies
vars <- iris[, -5]
apply(vars, 2, mean)

## Mediana: 50º quantil, de forma que divide os dados em duas metades
apply(vars, 2, median)

#Moda: valor mais frequente na amostra
freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)
freq_sl[1]

##Medidas de dispersão
#Variância: desvio da média
apply(vars, 2, var)

#Desvio padrão: raiz quadrada da variância
sd01 <- apply(vars, 2, sd)
# outra forma:
sd02 <- apply(vars, 2, function(x) sqrt(var(x)))
sd01
sd02
sd01 == sd02

##Coeficiente de variação: medida relativa de desvio padrão
#Não existe no R base uma função para calcular o coeficiente de variação. Isto não é um problema. Vamos formalmente criar nossa primeira função de R. Para isso, usamos a função function
cv <- function(x){
  sd(x)/mean(x)*100
}

apply(vars, 2, cv)

##Quantis ou percentis
#or padrão, a função quantile retorna o mínimo, o 25º percentil, a mediana, o 50º percentil, o 75º percentil e o máximo, também conhecidos pelo sumário de cinco números proposto por Tuckey. É possível modificar os percentis desejados com o argumento probs.

# sumario de 5 numeros
apply(vars, 2, quantile)
# 5%, 50% e 95%
apply(vars, 2, quantile, probs = c(0.05, 0.5, 0.95))

#Intervalo (range): O intervalo é a diferença entre o maior e o menor valor de determinada variável.

# a funcao range nos retorna os valores minimo e maximo
apply(vars, 2, range)
# aplicando a funcao diff ao resultado do range, temos o valor desejado
# uma boa pratica é nunca sobrescrever um objeto já existente no R, por isso nunca nomeie um objeto com um nome já existente
my_range <- function(x){
  diff(range(x))
}
apply(vars, 2, my_range)

##Intervalo interquartil (IIQ). O IIQ é a diferença entre o quartil superior (75%) e o quartil inferior (25%).
apply(vars, 2, IQR)

##Correlação
##Uma matriz de correlação é uma tabela com os valores de correlação entre cada uma das variáveis par a par. As variáveis podem ser correlacionadas positivamentes (valores positivos) ou negativamente (valores negativos). O que são variáveis altamente correlacionadas? Uma boa “regra de dedão” é que qualquer correlação 0,7 é considerada uma alta correlação.

cor(vars)



#################################
#########################
####### Métodos gráficos

###1. Gráfico de barras.
#Um gráfico de barras mostra a frequência de de observações em uma dada classe.
barplot(table(iris$Species))

## ps. "table" uses the cross-classifying factors to build a contingency table of the counts at each combination of factor levels.

###2. Histograma
#O histograma é o equivalente do gráfico de barras para variáveis contínuas. Cada barra representa um intervalo de valores. O número de intervalos pode ser especificado pelo usuário e afeta a percepção da distribuição dos dados.

par(mfrow = c(2, 2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)

par(mfrow = c(1, 1))

#Agora para o comprimento da sépala das espécies de Iris, vamos ver o efeito do número de intervalos no histograma com o argumento breaks.

par(mfrow = c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)

###3. Curva de densidade
#A curva de densidade mostra a probabilidade de observar determinado valor. Em comparação ao histograma, no eixo y, ao invés de termos a frequência, temos a densidade probabilística.

par(mfrow = c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, freq = FALSE)

#ps. freq --> logical; if TRUE, the histogram graphic is a representation of frequencies, the counts component of the result; if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one). Defaults to TRUE if and only if breaks are equidistant (and probability is not specified).

par(mfrow = c(1, 1))

# podemos ver a curva de densidade a usando a função por meio do plot da função density.

par(mfrow = c(1, 2))
# plot da curva de densidade
plot(density(iris$Sepal.Width))

# plot da curva de densidade sobre o histograma de densidade
hist(iris$Sepal.Width, freq = FALSE)
lines(density(iris$Sepal.Width), col = "blue") # note que agora estamos usando a funcao o comando add=TRUE
par(mfrow = c(1, 1))

##Box-plot ou box-whisker plot
#Box-plots são gráficos que representam o sumário de cinco números de Tuckey mostrando os quartis (25%, 50% e 75%), valores mínimos, máximo e outliers. O box-plot mostra a forma da distrubuição dos dados, a forma da distribuição e a habilidade de comparar com outras variáveis na mesma escala.

boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

#Agora vamos olhar para os valores por espécie.

boxplot(Sepal.Length ~ Species, data = iris)
boxplot(Sepal.Width ~ Species, data = iris)
boxplot(Petal.Length ~ Species, data = iris)
boxplot(Petal.Width ~ Species, data = iris)

#Vamos usar a própria função boxplot para identificar os outliers.

boxplot(iris$Sepal.Width)

my_boxplot <- boxplot(iris$Sepal.Width, plot = FALSE)
my_boxplot
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers <- my_boxplot$out
#qual a posicao dos outliers
which(iris$Sepal.Width %in% outliers)
# vamos usar a posicao para indexar o objeto
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]


#No caso anterior consideramos outliers em relação à distribuição da variável para todas as espécies juntas. É razoável assumir que cada espécie tenha um padrão morfométrico distinto de modo que poderíamos identificar outliers de maneira espécie específica.

boxplot(Sepal.Width ~ Species, data = iris)

my_boxplot2 <- boxplot(Sepal.Width ~ Species, data = iris, plot = FALSE)
my_boxplot2
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers2 <- my_boxplot2$out
# neste caso, queremos apenas os outliers da especie setosa
# vamos usar a posicao para indexar o objeto
iris[iris$Sepal.Width %in% outliers2 &
       iris$Species == "setosa",
     c("Sepal.Width", "Species")]

#####Entendendo a distribuição dos dados
#Vamos olhar para os dados morfométricos das espécies de Iris e comparar com uma distribuição normal. No R, isto pode ser feito de forma visual com as funções qqnorm e qqline.

par(mfrow = c(1,3))

qqnorm(iris$Sepal.Length[iris$Species == "setosa"],
       main = "setosa")
qqline(iris$Sepal.Length[iris$Species == "setosa"])
qqnorm(iris$Sepal.Length[iris$Species == "versicolor"],
       main = "versicolor")
qqline(iris$Sepal.Length[iris$Species == "versicolor"])
qqnorm(iris$Sepal.Length[iris$Species == "virginica"],
       main = "virginica")
qqline(iris$Sepal.Length[iris$Species == "virginica"])

par(mfrow = c(1,1))

##Relação entre variáveis
#Uma função no R que nos ajuda a explorar a relação entre muitas variáveis é a pairs. O resultado é uma matriz com variáveis em linhas e colunas o gráfico que vemos é o gráfico de dispersão para cada par de variáveis. A diagonal da matriz contém os nomes das variáveis. Note que o gráfico é espelhado de modo que a relação entre tamanho e comprimento de sépala aparece tanto na linha 1 e coluna 2 como na linha 2 e coluna 1.

pairs(vars)


#O pacote de R GGally fornece uma saída muito interessante para avaliar as relações entre variáveis pois já mostra o valor de correlação entre variáveis e a curva de densidade probabilística de cada uma das variáveis.

# EXEPCIONALMENTE vamos carregar o pacote agora, já que esse é um exercício bonus.
library("GGally")
ggpairs(vars)

##ps. toda vez que a gente bota a função par() seja lá o que tiver dentro, a gente ta botando o parâmetro/configuração pra toda a sessão de R.

##ps2. função expression() pra botar alpha, beta etc como letras gregas de fato. Ex. xlab = expression(paste(alpha, "1")) pra botar a letra grega alpha de fato como nome do eixo

#ps3. o abline() só funciona na sequencia de um gráfico.

#ps4. dev.off() apaga e cancela a visualzação dessa janela da direita. ex. gráficos. ela tb apaga qualquer configuração que  agente tenha feito pras janelas de plot


