# Script para manipulação de dados em bases relacionais ---#
# parte do curso Projetos de análise de dados em R
# dados originais extraídos de Jeliazkov et al 2020 Sci Data
# (https://doi.org/10.1038/s41597-019-0344-7)
# primeira versão em 2020-02-12
#-----------------------------------------------------------#

# carregando os pacotes necessários
library("tidyr")

#Uma função muito útil para a leitura de dados é a list.files() do pacote base. Esta função lista os arquivos em um diretório, baseado em um padrão. Vamos usar essa função para listar todos os arquivos em .csv do diretório.

files.path <- list.files(path = "data/cestes",
                         pattern = ".csv",
                         full.names = TRUE)

# Vamos usar o conteúdo desse vetor na função read.csv(). Há maneiras mais automatizadas de fazer a mesma tarefa cinco vezes! No arquivo .pdf da aula tem dois exemplos usando lapply e for para ler todas as planilhas uma única vez. Por enquanto, faremos um a um usando read.csv().

comm <- read.csv(files.path[1])
coord <- read.csv(files.path[2])
envir <- read.csv(files.path[3])
splist <- read.csv(files.path[4])
traits <- read.csv(files.path[5])

#head(comm)
#dim(comm)
#summary(comm)

#head(coord)
#dim(coord)
#summary(coord)

#head(envir)
#dim(envir)
#summary(envir)

#head(splist)
#dim(splist)
#summary(splist)

#head(traits)
#dim(traits)
#summary(traits)

#Temos dados de quantas espécies? Podemos simplesmente contar o número de linhas do objeto splist.
nrow(splist)

#Quantas áreas amostradas? Podemos contar o número de linhas dos objetos comm ou envir.
nrow(comm)
nrow(envir)

#Quantas variáveis ambientais?
# todas as variáveis exceto a primeira coluna com o id
names(envir)[-1]
# contando quantas variáveis
length(names(envir)[-1])

#Qual a riqueza de cada área? Primeiro, precisamos transformar a nossa matriz que possui dados de abundância em uma matriz de presença e ausência.
comm.pa <- comm[, -1] > 0

# vamos nomear as linhas das planilhas com o id dos sites
row.names(comm.pa) <- envir$Sites

#No R, os valores de TRUE e FALSE contam como 1 e 0. Vamos calcular a riqueza da área 1, por exemplo, somando a primeira linha do novo objeto comm.pa.
sum(comm.pa[1, ])

#Como podemos fazer a soma de forma automatizada para as 97 áreas? Podemos usar a função apply. Essa função aplica uma função às linhas ou colunas de um objeto (do tipo data.frame ou matrix).

rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum)
summary(rich)

#Vamos usar a função merge() do pacote base para adicionar a coluna de coordenadas ao objeto contendo as variáveis ambientais. Esta função irá combinar duas planilhas por meio de um identificador comum, que é a chave primária. No caso do objeto envir a chave primária é a coluna Sites que contém a numeração das localidades amostradas. Podemos chamar essa coluna usando o operador $.

envir$Sites

#São 97 áreas. Vamos ver o que acontece quando usamos a função summary().

summary(envir$Sites)

# se checarmos a classe desse vetor, veremos que é numerica
class(envir$Sites)
# queremos que seja uma variável categórica. Para isso, convertemos em fator
as.factor(envir$Sites)
# se usarmos apenas as.factor, não fazemos a conversão, vamos então fazer uma atribuição
envir$Sites <- as.factor(envir$Sites)

#Vamos fazer o mesmo para a variável Sites do objeto coord.
coord$Sites <- as.factor(coord$Sites)

#Juntando coord e envir
#Vamos então aplicar a função merge.
envir.coord <- merge(x = envir,
                     y = coord,
                     by = "Sites")
dim(envir)
dim(coord)
dim(envir.coord)
head(envir.coord)

#Agora, queremos transformar a nossa matriz de espécie vs. área em uma planilha que contenha cada observação em uma linha e cada variável em uma coluna. Cada observação é a abundância de uma espécie em uma determinada área. Para fazer essa transformação iremos usar a função gather() do pacote tidyr. Como temos 97 sites e 56 espécies, terminaremos com um objeto com 5432 linhas (97 x 56).

# vetor contendo todos os Sites
Sites <- envir$Sites
length(Sites)

# vetor número de espécies
n.sp <- nrow(splist)
n.sp

# criando tabela com cada especie em cada area especies em linhas
comm.df <- tidyr::gather(comm[, -1])

# nomes atuais
colnames(comm.df)
# modificando os nomes das colunas
colnames(comm.df) <-  c("TaxCode", "Abundance")
# checando os novos nomes
colnames(comm.df)

#Queremos agora adicionar a coluna Sites ao novo objeto. Vamos usar a função rep(). Esta função cria sequências. Vamos criar uma sequência de localidades, em que cada uma das 97 localidades se repete 56 vezes. A sequência deve ter também 5432 elementos.

# primeiro criamos a sequência
seq.site <- rep(Sites, times = n.sp)
# checando a dimensão
length(seq.site)
# adicionando ao objeto comm.df
comm.df$Sites <- seq.site
# checando como ficou
head(comm.df)

#Para terminar, vamos juntar splist, traits e envir.coord à planilha comm.df.

#Como vimos na aula, as relações entre duas tabelas são sempre feitas par a par. Então, vamos juntar par a par as tabelas usando a função merge().

#Primeiro, vamos adicionar as informações das espécies contidas em splist à comm.df usando a coluna TaxCode.

comm.sp <- merge(comm.df, splist, by = "TaxCode")
head(comm.sp)

#Segundo, adicionamos os dados de atributos das espécies à tabela de comunidade. Na tabela traits, a coluna que identifica as espécies é chamada Sp. Antes de fazer a junção, precisamos mudar o nome para bater com o nome da coluna em comm.sp que é TaxCode.

names(traits)
# renomeando o primeiro elemento
colnames(traits)[1] <- "TaxCode"
comm.traits <- merge(comm.sp, traits, by = "TaxCode")
head(comm.traits)

#Finalmente, juntamos as variáveis ambientais (que aqui já contém as coordenadas) à tabela geral da comunidade por meio da coluna Sites.

comm.total <- merge(comm.traits, envir.coord, by = "Sites")
head(comm.total)

#Por último, finalizamos nossa rotina de manipulação de dados exportando a planilha final modificada. Para isso, usamos a função write.csv().

write.csv(x = comm.total,
          file = "data/01_data_format_combined.csv",
          row.names = FALSE)

#ps. função view() muito melhor pra visualizar
# salvar em .rda um arquivo que queremos mas não precisando salvar o workspace mas poderemos precisar depois e carregar na area de trabalho. Assim ele não fica tão pesado: save(objeto, "caminho+nome.rda")


##Dever de casa
## instalar os pacotes hoje a noite Knitr ; rmarkdown e todas as dependencias
install.packages("knitr", dependencies = T)
install.packages("rmarkdown", dependencies = T)

##terminar o turorial de git
