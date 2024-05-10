# -------------------------------------------------------------------------
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
load(here::here("database","data_mult.rda"))

## TAREFA #

#' analise multivariada de cada site para verificar o comportamento das variáveis (09/05/2024)
#' repassar o geoprocessamento
#' solicitar aprender como se retira os valores de densidade do soilgrids

# -------------------------------------------------------------------------
