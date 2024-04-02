library(tidyverse)

# 1. Coleta de dados

set.seed(12)
values <- c(rnorm(50,mean = 22,sd = 7),rnorm(50,mean = 20,sd = 5))

group <- c(rep('A',50),rep('B',50))

population <- data.frame(group,values)

#population %>% dplyr::arrange(group) %>% dplyr::group_by(group) %>% dplyr::summarise_all(mean)

# 2. calcular a estatística de interesse: diferença entre as médias

# population[ population$group == "A", colnames(population) == "values"] #option 1
# population[population[,'group']=='A','values'] # option 2
# population %>% dplyr::filter(group == 'A') %>% dplyr::select(values) %>% unlist %>% as.vector # option 3

mean.A <- mean(population[population[,'group']=='A','values'])
mean.B <- mean(population[population[,'group']=='B','values'])

diff.obs <- mean.A-mean.B

cat('diferença entre os grupos A e B é de',diff.obs,'\n')

# 3. Encontrar a distribuição nula: embaralhar os valores ao longo da colunae recalcular a diferença

n = 1000

null <- vector("numeric", n)

for(i in 1:n){
  
  new.mean.A <- mean(sample(population[,'values'],30))
  new.mean.B <- mean(sample(population[,'values'],30))
  
  null[i] <- new.mean.A-new.mean.B
  
}

# 4. Determinar a proporção de valores iguais ou mais extremos do que a estatística de interesse

# primeira forma
proportion <- sum(null >= diff.obs)
proportion/1000*100

# segunda forma
mean(null >= diff.obs)*100

# histograma da distribucao nula
 
hist(null, probability = T,xlim = c(-4,4))
lines(density(null), col = "blue")
abline(v = diff.obs, col="red", lwd=2)

### normal distribution (determinar o p value)

1 - pnorm(diff.obs,mean(null),sd(null))

# -------------------------------------------------------------------------
## Outra forma transformando os valores da distribuição nula em valores normais

null.normal <- scale(null) %>% unlist %>% as.vector
diff.normalizada <- (diff.obs-mean(null))/sd(null)

hist(null.normal,probability = T)
lines(density(null.normal), col = "blue")
abline(v = diff.normalizada , col = "red", lwd = 2)

# determinar o valor do p.valor fazendo uso da tabela de 'Z normal'

1 - pnorm(diff.normalizada)

# -------------------------------------------------------------------------
#### Se eu quisesse utilizar a base de dados originais para fazer um hipótese
#' por exemplo, gostaria se saber qual é a probabilidade que a media da população seja 
#' maior ou igual a 25

media.hipotese <- 25
media.populacional <- population[,'values'] %>% mean

differenca.observada <- media.hipotese - media.populacional 

# determinar a distribuição nula

n = 1000

null <- vector("numeric", n)

for(i in 1:n){
  
  mean1 <- mean(sample(population[,'values'],30))
  mean2 <- mean(sample(population[,'values'],30))
  
  null[i] <- mean1-mean2
  
}

# Determinar a proporção de valores iguais ou mais extremos do que a estatística de interesse

# primeira forma
proportion <- sum(null >= differenca.observada)
proportion/1000*100 

# segunda forma
mean(null >= differenca.observada)*100

# histograma da distribucao nula

hist(null, probability = T,xlim = c(-4,4))
lines(density(null), col = "blue")
abline(v = differenca.observada, col = "red", lwd = 2)

### normal distribution (determinar o p value)

1 - pnorm(differenca.observada,mean(null),sd(null))

# -------------------------------------------------------------------------
## Se eu quisesse fazer a transformação dos valores 'null' em valores normais

null.normal <- scale(null) %>% unlist %>% as.vector
diff.normalizada <- (differenca.observada-mean(null))/sd(null)

hist(null.normal, probability = T)
lines(density(null.normal), col = "blue")
abline(v = diff.normalizada , col = "red", lwd = 2)

# determinar o valor do p.valor fazendo uso da tabela de 'Z normal'

1 - pnorm(diff.normalizada)

# -------------------------------------------------------------------------

#https://acervolima.com/histograma-de-sobreposicao-com-curva-de-densidade-ajustada-em-r/
#https://rpubs.com/victorpasson/normalizacao
#https://pt.wikihow.com/Calcular-o-Tamanho-de-uma-Amostra
# fazer uma formula para calcular de forma rapida, como determinar o tamanho de amostra.

# -------------------------------------------------------------------------

# pergunta: qual é probabilidade que a media populacional esteja entre 15 e 22

# identificamos os valores em estudo
media.1 <- 15
media.2 <- 22
values.populational <- population[,'values']
media.populacional <- population[,'values'] %>% mean
sd.populational <- population[,'values'] %>% sd
normal.populational <- population[,'values'] %>% scale %>% unlist %>% as.vector

# transformamos os valores da media em valores normais
normal.1 <- (media.1 - media.populacional)/sd.populational
normal.2 <- (media.2 - media.populacional)/sd.populational

hist(normal.populational, probability = T,xlim = c(-5,5))
lines(density(normal.populational), col = "blue")
abline(v = normal.1 , col = "green", lwd = 2)
abline(v = normal.2 , col = "red", lwd = 2)

pnorm(normal.2)-pnorm(normal.1)

# segundo forma
pnorm(22,mean(values.populational),sd(values.populational)) - pnorm(15,mean(values.populational),sd(values.populational)) 

probability.area <- pnorm(media.2,mean(values.populational),sd(values.populational)) - pnorm(media.1,mean(values.populational),sd(values.populational)) 

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# IMPORTANTE: talvez possamos trabalhar em uma função integrativa para resolver problemas de 
# normalidade com sombreamento das curvas. Poderia fazer umas pequenas funções.

# Agora pretendo fazer o mesmo exercicio mas sombreando as áreas que quero
# as funções foram utilizadas a partir do script 'tables_distribuition.R' que está
# localizada na pasta "france' e subpasta 'scripts'.
# para próximas consultas associdas à distribuição normal, consultar lá com outras funções.

# dados de distribution NORMAL STANDARD
#acima dele posso plotear o exercício que quero fazer.
set.seed(1)
dat <- with(density(rnorm(999999)), data.frame(x, y))

ggplot(data = dat, mapping = aes(x = x, y = y)) +
  
  geom_line() +
  
  geom_area(mapping = aes(x = ifelse(x>normal.1 & x<normal.2, x, NA))
            ,colour = 'blue',size = 0.5
            ,fill = "tomato"
            ,alpha = 0.5) +
  
  #aplicando a função editada desde a página de referencia e consulta
  geom_vdensity(data = dat, at = normal.1, color = "blue") + # primeiro corte
  geom_vdensity(data = dat, at = normal.2, color = "blue") + # segundo corte
  
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(dat$y))) +
  
  geom_text(aes(x = ((normal.2+normal.1)/2)
               ,y = (curve.limits(data = dat, at = normal.2)[2])/2
               ,label = paste0(sprintf("%.2f", (pnorm(normal.2)-pnorm(normal.1))*100),'%'))
            ,angle = 0) +
  
  theme_classic()


# -------------------------------------------------------------------------
# se fizesse com os dados originais
# a função with vai criar organizar um base de dados de acordo com a função especificada
# nesse caso com a função density, com os dados originais criamos os valos 
# de densidadade no eixo Y e os valores reais no eixo X

dat <- with(density(values.populational), data.frame(x, y))

ggplot(data = dat, mapping = aes(x = x, y = y)) +
  
  geom_line() +
  
  geom_area(mapping = aes(x = ifelse(x>media.1 & x<media.2, x, NA))
            ,colour = 'blue',size = 0.5
            ,fill = "tomato"
            ,alpha = 0.5) +
  
  geom_vdensity(data = dat, at = media.1, color = "blue") + # primeiro corte
  geom_vdensity(data = dat, at = media.2, color = "blue") +
  
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(dat$y))) +
  
  geom_text(aes(x = ((media.2+media.1)/2)
                ,y = (curve.limits(data = dat, at = media.2)[2])/2
                ,label = paste0(sprintf("%.2f",probability.area*100),'%'))
            ,angle = 0) 
  
# -------------------------------------------------------------------------
# para cortar a altura do segmento não se pode fazer com geom_vline
# deve-se fazer previamente uma pequena função,consultada em:
# https://stackoverflow.com/questions/73240747/cut-off-geom-vline-at-geom-density-height

geom_vdensity <- function(data, at, ...) {
  ggplot2::geom_segment(
    data = dplyr::filter(as.data.frame(density(data)[1:2]),
                         seq_along(x) == which.min(abs(x - at))),
    ggplot2::aes(x, 0, xend = x, yend = y), ...)}

#editado porque estava fazendo uma nova matrix de densidade e precisava usar a original.
#no entanto, se eu precisasse utilizar a base de dados original aí se deveriamos fazer isso.
geom_vdensity <- function(data, at, ...) {
  ggplot2::geom_segment(
    data = dplyr::filter(data,
                         seq_along(x) == which.min(abs(x - at))),
    ggplot2::aes(x, 0, xend = x, yend = y), ...)}


#função para determinar os limites de uma curva, tanto o X como o Y
curve.limits <- function(data,at){
  data %>% 
  dplyr::filter(seq_along(x) == which.min(abs(x - at))) %>% 
    unlist %>% 
    as.vector}

# -------------------------------------------------------------------------
# aplication example desde a páginas consultada para a 'geom_vdensity ' original e não editada.

library(ggplot2)

mtcars$car <- rownames(mtcars)

javelin <- mtcars[mtcars$car == "AMC Javelin", ]

#rodar a função tal como foi indicada na página.
ggplot(mtcars) +
  geom_density(aes(x = mpg)) +
  geom_vdensity(data = mtcars$mpg, at = javelin$mpg, color = "red")


#usando o exemplo com base de dados originais de mtcars, sem aplicação da função.
ggplot(mtcars) +
  geom_density(aes(x = mpg)) + #transforma os dados originais
  geom_segment(data = as.data.frame(density(mtcars$mpg)[1:2]) %>%
                 filter(seq(nrow(.)) == which.min(abs(x - javelin$mpg))),
               aes(x, 0, xend = x, yend = y))














