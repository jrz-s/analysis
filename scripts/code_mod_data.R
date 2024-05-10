##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' ECOSYS - UFS
#' rzaratesalazar@gmail.com
###################################################################

# -------------------------------------------------------------------------
# Load packages

library(conflicted)
library(tidyverse)

lis_lu <- read.table(choose.files(), h = T)

lis_lu %>% 
  tidyr::separate(col = time, into = c('year','mon')) %>% 
  dplyr::mutate(mon = mon %>% str_replace_na(replacement = '0')) %>% 
  dplyr::mutate(mon = recode(mon
                             ,'08' = '1'
                             ,'17' = '2'
                             ,'25' = '3'
                             ,'33' = '4'
                             ,'42' = '5'
                             ,'5' = '6'
                             ,'58' = '7'
                             ,'67' = '8'
                             ,'75' = '9'
                             ,'83' = '10'
                             ,'92' = '11'
                             ,'0' = '12')) %>% 
  dplyr::filter(year%in% c(9992,9993,10000,10001)
                ,mon %in% 1) 

# -------------------------------------------------------------------------
