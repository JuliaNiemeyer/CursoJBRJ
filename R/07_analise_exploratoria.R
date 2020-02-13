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
