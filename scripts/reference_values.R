library(conflicted)
library(tidyverse)
library(googlesheets4)
# -------------------------------------------------------------------------

url <- 'https://docs.google.com/spreadsheets/d/1fZGqHcRJ9PLExNcm7HaNVG0rVxHJLmGU-65CO9HXi48/edit#gid=1480410224'
gs <- as_sheets_id(url)
fb <- gs %>%
  range_read('Biomassa') 


db <- fb[,c(13:18,20:22)]
colnames(db) <- c('AGB','Folhas','Galho','Tronco','Raíz\ngrossa','Raíz\nfina','Serrapilheira','Madeira\nmorta','Madeira\nmorta caida')

db <- db %>% pivot_longer(cols = 1:9
                          ,names_to = 'compartimentos'
                          ,values_to = 'values')

fator <- c('AGB','Folhas','Galho','Tronco','Raíz\ngrossa','Raíz\nfina','Serrapilheira','Madeira\nmorta','Madeira\nmorta caida')
db$compartimentos <- factor(db$compartimentos,levels = fator,)

#db[,'n'] <- 'm'

# db <- db %>% 
#   replace(is.na(.),0)
# 
# db <- db %>% dplyr::filter(compartimentos %in% 'biomassa')

# -------------------------------------------------------------------------

p1 <- ggplot(data = db,mapping = aes(x = reorder(compartimentos,values),y = values))+
  theme_classic()+
  #coord_flip()+
  labs(title = 'Valores de referência da Biomassa'
       ,subtitle = 'ECOSYS, UFS - 28/03/2024\n')+
  xlab('Biomassa')+
  ylab('Estoques de carbono (Mg/ha)')+
  scale_y_continuous(expand = c(0,0)
                     ,limits = c(0,160)
                     ,breaks = seq(0,160,160/5)) +
  stat_summary(fun.data = 'mean_sdl', fun.args = list(mult = 1), 
               geom = "errorbar", color = "grey50", width = 0.2, na.rm = TRUE)+
  # stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), 
  #              geom = "crossbar", width = 0.5, na.rm = TRUE)
  # geom_errorbar(stat = "summary", fun.data = "mean_se",
  #               position = position_dodge(width = 0.9), width = 0.15, na.rm = TRUE) #+
  geom_crossbar(stat = "summary", fun = "mean", aes(ymin = ..y.., ymax = ..y..), color = 'grey50',
                fatten = 0.2, width = 0.5, position = position_dodge(width = 0.9),
                show.legend = F, na.rm = TRUE) +
  geom_point(na.rm = TRUE)+
  geom_text(aes(label = after_stat(sprintf("%.2f", y)))
            ,stat = "summary"
            ,fun = "mean"
            ,vjust = -0.5
            ,hjust = -0.5
            ,na.rm = TRUE
            ,size = 3
            ,color = 'blue')+
  theme(plot.margin = ggplot2::unit(c(0.7, 0.7, 0.7, 0.7), "cm")
        ,axis.title.x = element_text(vjust = -1, size = 15)
        ,text = element_text(family = 'serif')
        ,axis.title.y = element_text(size = 15)
        ,axis.text = element_text(size = 13))

p1

#https://stackoverflow.com/questions/74704444/how-to-add-y-value-average-text-to-geom-bar
#http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization

# -------------------------------------------------------------------------

w.p <- 1500
h.p <- 1000

png(  here::here("figures", "biomassa.png")
      ,width = w.p
      ,height = h.p
      ,res = 110 #em ppi (pixel per inches), modifica a qualidade
      ,family = "serif")

p1

dev.off()

# -------------------------------------------------------------------------

url <- 'https://docs.google.com/spreadsheets/d/1fZGqHcRJ9PLExNcm7HaNVG0rVxHJLmGU-65CO9HXi48/edit#gid=1480410224'
gs <- as_sheets_id(url)
fb <- gs %>%
  range_read('Solo') 

db <- fb[,13:18] %>% as.data.frame %>% as_tibble

colnames(db) <- c('areia','silte','argila','Ds','pH','COS')

db$areia <- c(db$areia %>% unlist %>% as.vector,NA)

colnames(db) <- c('Areia\n(dag/kg)','Silte\n(dag/kg)','Argila\n(dag/kg)','Ds\n(Mg/m³)','pH','COS\n(Mg/ha)')

db <- db %>% dplyr::mutate(across(.cols = c(1:ncol(db)), .fns = ~as.numeric(.)))

db <- db %>% pivot_longer(cols = 1:6
                          ,names_to = 'variaveis'
                          ,values_to = 'values')

fator <- c('Areia\n(dag/kg)','Silte\n(dag/kg)','Argila\n(dag/kg)','Ds\n(Mg/m³)','pH','COS\n(Mg/ha)')
db$variaveis <- factor(db$variaveis,levels = fator)

# -------------------------------------------------------------------------

p2 <- ggplot(data = db,mapping = aes(x = variaveis,y = values))+
  #facet_wrap(.~variaveis)+
  theme_classic()+
  #coord_flip()+
  labs(title = 'Valores de referência do solo'
       ,subtitle = 'ECOSYS, UFS - 28/03/2024\n')+
  xlab('Parâmetros do solo')+
  ylab('Valores')+
  scale_y_continuous(expand = c(0,0)
                     ,limits = c(0,100)
                     ,breaks = seq(0,100,100/5)) +
  stat_summary(fun.data = 'mean_sdl', fun.args = list(mult = 1), 
               geom = "errorbar", color = "grey50", width = 0.2, na.rm = TRUE)+
  # stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), 
  #              geom = "crossbar", width = 0.5, na.rm = TRUE)
  # geom_errorbar(stat = "summary", fun.data = "mean_se",
  #               position = position_dodge(width = 0.9), width = 0.15, na.rm = TRUE) #+
  geom_crossbar(stat = "summary", fun = "mean", aes(ymin = ..y.., ymax = ..y..), color = 'grey50',
                fatten = 0.2, width = 0.5, position = position_dodge(width = 0.9),
                show.legend = F, na.rm = TRUE) +
  geom_point(na.rm = TRUE)+
  geom_text( aes(label = after_stat(sprintf("%.2f", y)))
            ,stat = "summary"
            ,fun = "mean"
            ,vjust = -0.8
            ,hjust = -0.5
            ,na.rm = TRUE
            ,size = 3
            ,color = 'blue')+
  theme(plot.margin = ggplot2::unit(c(0.7, 0.7, 0.7, 0.7), "cm")
        ,axis.title.x = element_text(vjust = -1, size = 15)
        ,text = element_text(family = 'serif')
        ,axis.title.y = element_text(size = 15)
        ,axis.text = element_text(size = 13))

# -------------------------------------------------------------------------

w.p <- 1500
h.p <- 1000

png(  here::here("figures", "solo.png")
      ,width = w.p
      ,height = h.p
      ,res = 110 #em ppi (pixel per inches), modifica a qualidade
      ,family = "serif")

p2

dev.off()


