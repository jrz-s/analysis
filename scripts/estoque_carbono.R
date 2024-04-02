# Função para transformar o teor de carbono (g/kg) do solo em Tn C / ha

#argumentos
# ds = g/cm³
# prof = metros
# cos = g C/kg de solo

estoque_carbono <- function(ds,prof,cos){
  
  a <- 1000/ds # quantos cm³ contém um 1kg de solo
  
  b <- a*10^(-6) # quantos m³ contém um 1kg de solo
  
  c <- b/prof # quantos m² ocupa 1kg de solo em uma profundidade de 0.2m
  
  d <- (cos/c)*10^(-2) # quantas toneladas de C tem um hectare
  
  d
  
}


estoque_carbono2 <- function(ds,prof,cos){
  
  v <- 10000 * prof # volume do solo em m³ em uma hectare e uma profundidade do solo
  m <- v * ds # massa do solo em uma hectare a uma profundade 
  c <-  cos * m * 10^(-3) # quantas toneladas de C tem um hectare
  
  c
  
}


#estoque_carbono2(ds = 1.49,prof = 0.2,cos = 19.97)
