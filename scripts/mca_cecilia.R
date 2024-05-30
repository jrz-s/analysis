##################################################################
#' Zárate-Salazar, J. Rafael 
#' Agronomy | Biodiversity - MS | Soil Science - PhD
#' ECOSYS - UFS
#' rzaratesalazar@gmail.com
##################################################################

# -------------------------------------------------------------------------
# Load packages -----------------------------------------------------------

library(FactoMineR)
library(factoextra)
library(tidyverse)

# -------------------------------------------------------------------------
# Load data ---------------------------------------------------------------

db <- readxl::read_xlsx(path = here::here('database','mca.xlsx'),sheet = 3) %>% 
  dplyr::mutate(arruda = `arruda...7`+`arruda...8`) %>% 
  dplyr::select(!c(`arruda...7`,`arruda...8`)) %>% 
  tibble::column_to_rownames('diseases') %>% as.matrix

db <- db[ names(sort(apply(db,1,sum),decreasing = F))
        ,names(sort(apply(db,2,sum),decreasing = F))]

# -------------------------------------------------------------------------

library("gplots")
# 1. convert the data as a table
dt <- as.table(as.matrix(db))
# 2. Graph
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

chisq <- chisq.test(db)
chisq

library("FactoMineR")
res.ca <- CA(db, graph = FALSE)
library("factoextra")
eig.val <- get_eigenvalue(res.ca)
eig.val
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 10))
fviz_ca_biplot(res.ca, repel = TRUE)

library(bipartite)
plotweb( db
         ,text.rot = 90
         ,high.lablength = 3
         ,low.lablength = 20
         ,arrow = "both.center"
         ,y.lim = c(-0.1,1.8)
         ,labsize = 1.3
         ,col.high = "black"
         ,col.low = "black"
         ,y.width.low = 0.05
         ,y.width.high = 0.05
         ,text.low.col = "black"
         ,text.high.col = "black"
         ,adj.high = 0.5
         ,adj.low = 0.9
         ,bor.col.interaction ="black"
         ,method = "normal")


colnames(db)

