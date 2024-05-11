##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' ECOSYS - UFS
#' rzaratesalazar@gmail.com
###################################################################

# load database
library(tidyverse)

load(here::here("database","data_mult.rda"))

colnames(data_mult)

mdl <- lm(cos~tmin+tmax+precip+sand+silt+clay+ds+ph,data = data_mult)
summary(mdl)

cor <- agricolae::correlation(data_mult[,2:10])

cor.value <- cor$correlation
sig.value <- cor$pvalue

# -------------------------------------------------------------------------

# Ánálise VIF (variance inflation factor)
# avaliar a multicolinearidade
# interpretação: https://www.linkedin.com/pulse/vifvariance-inflation-factor-thiago-teles/

car::vif(mdl)

mdl <- lm(cos ~ tmin + tmax + precip + silt + ds + ph, data = data_mult)
car::vif(mdl)
summary(mdl)

# -------------------------------------------------------------------------

par(mfrow= c(2,2))
plot(mdl)

# -------------------------------------------------------------------------

shapiro.test(data_mult$tmin)
shapiro.test(data_mult$tmax)
shapiro.test(data_mult$precip) # olhar
shapiro.test(data_mult$sand)
shapiro.test(data_mult$silt) # olhar
shapiro.test(data_mult$clay) # olhar
shapiro.test(data_mult$ds)
shapiro.test(data_mult$ph) # olhar

# -------------------------------------------------------------------------

normality.id <- function(data, var, p.value){
  db <- data[,var]
  my_matrix <- matrix(nrow = ncol(db),ncol = 1)
  colnames(my_matrix) <- 'shapiro'
  p <- length(var)
  
  for(i in 1:p){
    my_matrix[i,1] <- shapiro.test(db[,i] %>% unlist %>% as.vector) %>% 
      purrr::pluck('p.value') %>% round(digits = 4)}
  
  my_matrix <- my_matrix %>% as.data.frame %>% rowid_to_column(var = 'variables') %>% 
    dplyr::mutate(normal = ifelse(shapiro>=p.value,'yes','no'))
  my_matrix[,'variables'] <- colnames(data_mult)[var]
  my_matrix}

# -------------------------------------------------------------------------

normality.id(data = data_mult, var = 2:10, p.value = 0.01)

# -------------------------------------------------------------------------

gmdl <- glm(cos ~ tmin + tmax + precip + silt + ds + ph, data = data_mult)

step(gmdl)

mdl <- lm(cos ~ tmax + precip, data = data_mult)
summary(mdl)
aov(mdl)
anova(mdl)

# -------------------------------------------------------------------------
#https://en.wikipedia.org/wiki/Ridge_regression
