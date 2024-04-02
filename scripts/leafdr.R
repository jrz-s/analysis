# -------------------------------------------------------------------------

library(conflicted)
library(tidyverse)
library(ggtext)

# -------------------------------------------------------------------------

db <- readxl::read_xlsx(path = here::here('database','leafdr.xlsx'))

colnames(db) <- c('group','leafdr','leaf.value','prec.value')

db <- db %>% 
  dplyr::mutate(leafdr = db %>% 
                  dplyr::select(leafdr) %>% 
                  unlist %>% as.vector %>% 
                  str_extract(pattern = '[0-9]+'))

db$leafdr <- as.numeric(db$leafdr)

axis_one_max <- max(db$leaf.value)
axis_two_max <- max(db$prec.value)
scaleFactor <- axis_two_max/axis_one_max

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

p1 <- ggplot( data = db
       ,mapping = aes( x = leafdr
                      ,y = leaf.value)) +
  
  # Adição das linhas do eixo 1
  #geom_point() +
  geom_line(linewidth = 0.5,colour = 'red', aes(linetype = 'Leafdr')) +
  
  # Adição das linhas do eixo 2
  geom_line( mapping = aes(y = prec.value/scaleFactor,linetype = 'Precipitation')
            ,linewidth = 0.5,colour = 'blue') +

  facet_wrap(.~group)+
  scale_x_continuous(expand = c(0,0.5),
                     limits = c(NA,12),
                     breaks = seq(1,12,1),
                     name = 'LEAFDR') +
  
  scale_y_continuous(
     name = "Leafdr <br> <span style = 'color:grey20'><span style = 'font-size:13pt'>(values)</span>"
    ,expand = c(0,0)
    ,limits = c(0,0.2)
    ,breaks = seq(0,0.2,0.2/5)
    
    ,sec.axis = sec_axis(
       name = "Precipitation <br> <span style = 'color:grey20'><span style = 'font-size:13pt'>(mm)</span>"
      ,trans = ~.*scaleFactor
      ,breaks = round(seq(0,222,222/5),0))) +
  
  theme_light() +
  theme(    axis.title.y.left =  element_textbox_simple(size = 15,
                                                        family = "serif", #sans e mono
                                                        hjust = 0.5,
                                                        halign = 0.5,
                                                        orientation = 'left-rotated',
                                                        minheight = unit(0.55, "in"),
                                                        fill = "white"),  
            
            axis.title.y.right = element_textbox_simple(size = 15,
                                                        family = "serif", #sans e mono
                                                        hjust = 0.5,
                                                        halign = 0.5,
                                                        orientation = 'right-rotated',
                                                        minheight = unit(0.55, "in"),
                                                        fill = "white"), 
            
            #modifica só o nome e tamanho do nome do eixo X
            axis.title.x = element_text(colour = "black", 
                                        family = "serif", #sans e mono
                                        vjust = -1, 
                                        size = 13)
            ,legend.text = element_text(size = 10)
            ,legend.title = element_text(size = 10,face = 'bold')
            ,legend.key.size = unit(0.55, "cm")
            ,legend.position = c(0.6,0.95)
            ,strip.background = element_rect(fill = "#474747",color = "#474747", linewidth = 1)
            ,strip.text = element_textbox_simple(size = rel(0.8)
                                                 , color = "white"
                                                 ,halign = 0.5
                                                 ,valign = 0.5
                                                 ,height = 0.03)
            ,panel.border = element_rect(colour = "grey30", linewidth = 0.05) #dando problemas
            ,panel.background = element_rect(fill = "transparent")
            ,panel.grid.major = element_line("white")
            ,panel.grid.minor = element_line("white")
            ,plot.margin = ggplot2::unit(c(0.5,0.5,0.5,0.5), "cm")) +
  
  guides(linetype = guide_legend(title = NULL))

# -------------------------------------------------------------------------

w.p <- 1200
h.p <- 1000

png(  here::here("figures", "leafdrxprec.png")
      ,width = w.p
      ,height = h.p
      ,res = 110 #em ppi (pixel per inches), modifica a qualidade
      ,family = "serif")

p1

dev.off()

# -------------------------------------------------------------------------
# material de consulta
#https://www.tidyverse.org/blog/2024/02/ggplot2-3-5-0-legends/

# -------------------------------------------------------------------------
