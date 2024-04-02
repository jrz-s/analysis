library(conflicted)
library(tidyverse)
library(ggtext)

mm <- readxl::read_xlsx(path = here::here('database','ma_geral.xlsx'))

mm[,3:5] <- mm[,3:5]*100

colnames(mm) <- c('ponto','type','Areia (dag/kg)','Silte (dag/kg)','Argila (dag/kg)','Ds (Mg/m³)','pH','ppt (mm)','Tmin (ºC)','Tmax (ºC)','COS (Mg/ha)')
mm <- mm[,-1]

mm <- mm %>% tidyr::pivot_longer(cols = -1
                           ,names_to = 'variables'
                           ,values_to = 'values')

fator <- c('ponto','type','Areia (dag/kg)','Silte (dag/kg)','Argila (dag/kg)','Ds (Mg/m³)','pH','COS (Mg/ha)','ppt (mm)','Tmin (ºC)','Tmax (ºC)')
mm$variables <- factor(mm$variables,levels = fator,)

# -------------------------------------------------------------------------

p1 <- ggplot(data = mm
       ,mapping = aes(x = type,y = values)) +
  geom_point(na.rm = T)+
  facet_wrap(.~variables,scales = "free_y")+
  xlab('Grupos funcionais') +
  ylab('Valores de referência')+
  labs(title = 'Grupos funcionais vs. Variáveis Edafoclimáticas'
       ,subtitle = 'ECOSYS, UFS - 28/03/2024\n')+
  stat_summary(fun.data = 'mean_sdl', fun.args = list(mult = 1), 
               geom = "errorbar", color = "grey50", width = 0.2, na.rm = TRUE)+
  geom_crossbar(stat = "summary", fun = "mean", aes(ymin = ..y.., ymax = ..y..), color = 'grey50',
                fatten = 0.2, width = 0.5, position = position_dodge(width = 0.9),
                show.legend = F, na.rm = TRUE)+
  geom_text(aes(label = after_stat(sprintf("%.2f", y)))
            ,stat = "summary"
            ,fun = "mean"
            ,vjust = -0.5
            ,hjust = -0.5
            ,na.rm = TRUE
            ,size = 3
            ,color = 'blue')+
  theme_light() +
  theme(
    strip.background = element_rect(fill = "#474747",color = "#474747", linewidth = 1)
    ,strip.text = element_textbox_simple(size = rel(1)
                                         , color = "white"
                                         ,halign = 0.5
                                         ,valign = 0.5
                                         ,height = 0.04)
    ,panel.border = element_rect(colour = "grey30", linewidth = 0.05) #dando problemas
    ,panel.background = element_rect(fill = "transparent")
    ,panel.grid.major = element_line("white")
    ,panel.grid.minor = element_line("white")
    ,text = element_text(family = "serif")
    ,axis.title.y = element_text(size = 15)
    ,axis.title.x = element_text(vjust = -3, size = 15)
    ,axis.text.y = element_text(size = 8, color = 'black')
    ,axis.text.x = element_text(size = 10, color = 'black')
    ,axis.line = element_line(colour = "grey30")
    ,axis.ticks = element_line(colour = "grey30")
    ,legend.text = element_text(size = 11)
    ,legend.title = element_text(size = 11, face = 'bold')
    ,legend.spacing.y = unit(0.2,'cm')
    ,legend.key.size = unit(0.8, units = "cm")
    ,plot.margin = ggplot2::unit(c(0.7, 0.7, 0.7, 0.7), "cm"))

p1

# -------------------------------------------------------------------------

w.p <- 1500
h.p <- 1000

png(  here::here("figures", "dominioxedafo.png")
      ,width = w.p
      ,height = h.p
      ,res = 110 #em ppi (pixel per inches), modifica a qualidade
      ,family = "serif")

p1

dev.off()


# -------------------------------------------------------------------------
# Comparison with density
## script consulting is 'freq_plot.R' in Project 'france' and file 'scripts'

ma <- readxl::read_xlsx(path = here::here('database','ma_geral.xlsx'))
#ma <- readxl::read_xlsx(path = choose.files())
ma <- ma[,c(2,10)]

library(viridis)

ggplot(
  ma, # base de dados da mata atlântica
  aes(x = tmax, y = tipe)) +
  
  geom_density_ridges_gradient(
    aes(fill = after_stat(x)), scale = 3, size = 0.3) +
  
  scale_fill_gradientn(
   #colours = viridis(3) # para definir a escala de cores que abrange
    colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF")
   ,breaks = c(round(seq(24,32,(32-24)/4),1)) #para definir os valores de corte
   ,name = "Temperatura máxima")+
  
  labs(title = 'Temperatures in Lincoln NE') 

