##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' ECOSYS - UFS
#' rzaratesalazar@gmail.com
###################################################################

# Load packages -----------------------------------------------------------

library(conflicted)
library(tidyverse)
library(googlesheets4)
library(agricolae)

# -------------------------------------------------------------------------
# Base de datos ->

url <- 'https://docs.google.com/spreadsheets/d/1fZGqHcRJ9PLExNcm7HaNVG0rVxHJLmGU-65CO9HXi48/edit#gid=1480410224'
gs <- as_sheets_id(url)

# -------------------------------------------------------------------------
# Dados ambientais --------------------------------------------------------

fb <- gs %>%
  range_read("rascunho")

solo <- fb

# -------------------------------------------------------------------------

clima <- read.csv(file = choose.files())
clima <- clima[,-1]

clima.summary <- clima[,-c(2,5)] %>% 
  dplyr::group_by(site) %>% 
  dplyr::arrange(site) %>% 
  dplyr::summarise_all(mean) %>% 
  dplyr::inner_join(clima[,-c(2:4)] %>% 
                      dplyr::group_by(site) %>% 
                      dplyr::arrange(site) %>% 
                      dplyr::summarise_all(sum),by = 'site') %>% 
  dplyr::filter(!site %in% 'avg') %>% 
  tibble::add_row(clima.summary %>% dplyr::filter(site %in% c( 'CRACE'
                                                               ,'FOZPR'
                                                               ,'TERPR'
                                                               ,'ITABA'
                                                               ,'ITAPE'
                                                               ,'RECPE'
                                                               ,'UBASP')) %>% 
                    dplyr::mutate(site = paste0(site,2))) %>% 
  tibble::add_row(clima.summary %>% dplyr::filter(site %in% 'ITABA') %>% 
                    dplyr::mutate(site = paste0(site,3)))
  
# -------------------------------------------------------------------------

solo$ds <- solo$ds %>% unlist %>% as.vector('numeric')
solo$ph <- solo$ph %>% unlist %>% as.vector('numeric')
solo$cos <- solo$cos %>% unlist %>% as.vector('numeric')

solo <- solo %>% dplyr::rename('site' = code)

# -------------------------------------------------------------------------

data_mult <- clima.summary %>% dplyr::inner_join(solo,by = 'site')

save(data_mult, file = here::here("database","data_mult.rda"))

## TAREFA #

#' analise multivariada de cada site para verificar o comportamento das variáveis (09/05/2024)
#' repassar o geoprocessamento
#' solicitar aprender como se retira os valores de densidade do soilgrids

# -------------------------------------------------------------------------

data_mult <- data_mult %>% 
  dplyr::inner_join(solo[,1:2] %>% 
                      dplyr::rename('site' = code)
                    ,by = 'site')

save(data_mult, file = here::here("database","data_mult.rda"))

load(here::here("database","data_mult.rda"))

# -------------------------------------------------------------------------

# Analise de componentes principais

library(FactoMineR)
library(factoextra)

db <- data_mult %>% column_to_rownames(var = 'site')

res.pca <- PCA(db[,-c(10,11)]
              ,scale.unit = T 
              ,graph = F
              ,quali.sup = 10)

# -------------------------------------------------------------------------

summary(res.pca)

# -------------------------------------------------------------------------

p2 <- fviz_pca_biplot( res.pca 
                ,addEllipses = T
                ,habillage = "dominio"
                ,col.var = "black"
                ,xlim = c(-4,4)
                ,ylim = c(-4,4)
                ,title = "") +
  
  theme( panel.background = element_rect(fill="white",colour = "grey38")
         ,panel.grid.major = element_line("white")
         ,panel.grid.minor = element_line("white")
         ,axis.title.x = element_text(colour = "black")
         ,axis.title.y = element_text(colour = "black")
         ,axis.text.x = element_text(colour = "black")
         ,axis.text.y = element_text(colour = "black")
         ,legend.title = element_text(colour = "black")
         ,legend.text = element_text(colour = "black", size = 10)
         ,legend.position = c(0.90,0.15)
         ,legend.spacing.y = unit(0.2,'cm')
         ,legend.key.size = unit(0.45, units = "cm")
         ,legend.background = element_rect(fill = "white", colour = "grey38")
         ,plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  
  guides(color = "none"
         ,fill = 'none'
         ,shape = guide_legend(  title = NULL
                                ,override.aes = list(size = c(3,3,3,3) 
                                                    ,shape = c(22,22,22,22)
                                                    ,fill = c("#00BA38","tomato","#619CFF","orange")
                                                    ,color = c("#00BA38","tomato","#619CFF","orange")))) +
  
  scale_color_manual(values = c("#00BA38","tomato","#619CFF","orange")) +
  
  scale_fill_manual(values = c("#00BA38","tomato","#619CFF","orange")) +
  
  scale_shape_manual(values = c(15,15,15,15))  

# ------------------------------------------------------------------------

library(cowplot)

g1 <- cowplot::plot_grid(p1, p2, ncol = 2, labels = c('A', 'B'), rel_widths = c(1, 1))

# ------------------------------------------------------------------------

w.p <- 1500
h.p <- 800

png(  here::here("figures","pca.dominio_location.png")
      ,width = w.p
      ,height = h.p
      ,res = 110 #em ppi (pixel per inches), modifica a qualidade
      ,family = "serif")

g1

dev.off()

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

db2 <- db %>% dplyr::mutate(dominio = recode(dominio
                                             ,NEDEC = 'Deciduous'
                                             ,SEDEC = 'Deciduous'
                                             ,NEGREEN = 'Evergreen'
                                             ,SEGREEN = 'Evergreen'))

res.pca <- PCA(db2[,-c(10,11)]
               ,scale.unit = T 
               ,graph = F
               ,quali.sup = 10)

# -------------------------------------------------------------------------

p2 <- fviz_pca_biplot( res.pca 
                 ,addEllipses = T
                 ,habillage = "dominio"
                 ,col.var = "black"
                 ,xlim = c(-4,4)
                 ,ylim = c(-4,4)
                 ,title = "") +
  
  theme( panel.background = element_rect(fill="white",colour = "grey38")
         ,panel.grid.major = element_line("white")
         ,panel.grid.minor = element_line("white")
         ,axis.title.x = element_text(colour = "black")
         ,axis.title.y = element_text(colour = "black")
         ,axis.text.x = element_text(colour = "black")
         ,axis.text.y = element_text(colour = "black")
         ,legend.title = element_text(colour = "black")
         ,legend.text = element_text(colour = "black", size = 10)
         ,legend.position = c(0.90,0.15)
         ,legend.spacing.y = unit(0.2,'cm')
         ,legend.key.size = unit(0.45, units = "cm")
         ,legend.background = element_rect(fill = "white", colour = "grey38")
         ,plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  
  guides( color = "none"
         ,fill = 'none'
         ,shape = guide_legend(  title = NULL
                                ,override.aes = list(size = c(3,3) 
                                                     ,shape = c(22,22)
                                                     ,fill = c("tomato","#619CFF")
                                                     ,color = c("tomato","#619CFF")))) +
  
  scale_color_manual(values = c("tomato","#619CFF")) +
  
  scale_shape_manual(values = c(15,15))  

# ------------------------------------------------------------------------

library(cowplot)

g1 <- cowplot::plot_grid(p1, p2, ncol = 2, labels = c('A', 'B'), rel_widths = c(1, 1))

# ------------------------------------------------------------------------

w.p <- 1500
h.p <- 800

png(  here::here("figures","pca.dominio.png")
      ,width = w.p
      ,height = h.p
      ,res = 110 #em ppi (pixel per inches), modifica a qualidade
      ,family = "serif")

g1

dev.off()

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

db3 <- db %>% dplyr::mutate(dominio = recode(dominio
                                             ,NEDEC = 'Northeast'
                                             ,SEDEC = 'Southeast'
                                             ,NEGREEN = 'Northeast'
                                             ,SEGREEN = 'Southeast'))

res.pca <- PCA(db3[,-c(10,11)]
               ,scale.unit = T 
               ,graph = F
               ,quali.sup = 10)

# -------------------------------------------------------------------------

p2 <- fviz_pca_biplot( res.pca 
                 ,addEllipses = T
                 ,habillage = "dominio"
                 ,col.var = "black"
                 ,xlim = c(-4,4)
                 ,ylim = c(-4,4)
                 ,title = "") +
  
  theme( panel.background = element_rect(fill="white",colour = "grey38")
         ,panel.grid.major = element_line("white")
         ,panel.grid.minor = element_line("white")
         ,axis.title.x = element_text(colour = "black")
         ,axis.title.y = element_text(colour = "black")
         ,axis.text.x = element_text(colour = "black")
         ,axis.text.y = element_text(colour = "black")
         ,legend.title = element_text(colour = "black")
         ,legend.text = element_text(colour = "black", size = 10)
         ,legend.position = c(0.80,0.15)
         ,legend.spacing.y = unit(0.2,'cm')
         ,legend.key.size = unit(0.45, units = "cm")
         ,legend.background = element_rect(fill = "white", colour = "grey38")
         ,plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  
  guides( color = "none"
          ,fill = 'none'
          ,shape = guide_legend(  title = NULL
                                  ,override.aes = list(size = c(3,3) 
                                                       ,shape = c(22,22)
                                                       ,fill = c("#00BA38","orange")
                                                       ,color = c("#00BA38","orange")))) +
  
  scale_color_manual(values = c("#00BA38","orange")) +
  
  scale_fill_manual(values = c("#00BA38","orange")) +
  
  scale_shape_manual(values = c(15,15))  

# ------------------------------------------------------------------------

library(cowplot)

g1 <- cowplot::plot_grid(p1, p2, ncol = 2, labels = c('A', 'B'), rel_widths = c(1, 1))

# ------------------------------------------------------------------------

w.p <- 1500
h.p <- 800

png(  here::here("figures","pca.location.png")
      ,width = w.p
      ,height = h.p
      ,res = 110 #em ppi (pixel per inches), modifica a qualidade
      ,family = "serif")

g1

dev.off()

# ------------------------------------------------------------------------
writexl::write_xlsx(x = data_mult,path = here::here('database','data_clima_mult.xlsx'))
# ------------------------------------------------------------------------
