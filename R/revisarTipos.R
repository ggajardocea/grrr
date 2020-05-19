revisarTipos <- function(df1, df2){
  require(dplyr)
  require(magrittr)
  clases1 <- sapply(df1, function(x){class(x)}) %>% as.data.frame()
  clases1 <- data.table::setDT(clases1, keep.rownames = TRUE)[]
  clases2 <- sapply(df2, function(x){class(x)}) %>% data.frame()
  clases2 <- data.table::setDT(clases2, keep.rownames = TRUE)[]
  revision <- clases1 %>% dplyr::left_join(clases2, by = c("rn"))
  malos <- revision %>% dplyr::filter(..x != ..y)
  return(malos)
}
