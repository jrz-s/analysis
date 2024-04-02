# esta parte rodar dentro da pasta 2.get_multfiles
# -------------------------------------------------------------------------

m <- get_multfiles.original(
   century_dir = 'century4.5'
  ,tree.scenario = 'TREE06'
  ,name.obs = 'luquillo')

mm <- m[,1:3]

# -------------------------------------------------------------------------

n.simulation = 2
tree = TRUE
fix = TRUE
tree.name.obs = 'ECOSYS'

if(tree == TRUE){

n <- get_modification(
   century_dir = 'century4.5'
  ,file100 = 'tree'
  ,scenario = 'TREE06'
  ,PRDX2 = 0.88
  ,LEAFDR = c(0.09,0.06,0.05,0.05,0.05,0.04,0.08,0.10,0.18,0.11,0.13,0.09))

n <- n[-1] %>% as_tibble_col('variables')
n[,'row'] <- 1:nrow(n)+1
n[,'file'] <- rep('tree',nrow(n))

nn <- n

}

# -------------------------------------------------------------------------

if(fix == TRUE){
  
  n <- get_modification(
    century_dir = 'century4.5'
    ,file100 = 'fix'
    ,scenario = 'FIX01'
    ,VARAT1_2.1 = 5)
  
  n <- n[-1] %>% as_tibble_col('variables')
  n[,'row'] <- 1:nrow(n)+1
  n[,'file'] <- rep('fix',nrow(n))
  
  n2 <- n
  
  nn <- nn %>% dplyr::add_row(n2)
  
}

# -------------------------------------------------------------------------

p <- nn %>% anti_join(mm,by = join_by(variables, row, file))

p[,'simulation'] <- rep(n.simulation,nrow(p))
p[,'obs.'] <- rep(name.obs,nrow(p))

View(p)

# -------------------------------------------------------------------------

v <- m %>% dplyr::add_row(p)

# -------------------------------------------------------------------------

View(v)

# -------------------------------------------------------------------------
