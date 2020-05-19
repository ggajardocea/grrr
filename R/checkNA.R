checkNA <- function(df, view = 1){
  tabla <- sapply(df, function(x){mean(x %in% c(NA, Inf, -Inf, NaN))})
  if(view){
    View(tabla)
  } else{
    return(tabla)
  }
}
