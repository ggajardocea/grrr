monthSequence <- function(mes = 201910, lag = 3){
  mes1 <- lubridate::ymd(paste0(mes, "01"))
  mes2 <- mes1 + lubridate::months(lag)
  seq_meses <- seq(mes1, mes2, by='months')
  meses <- as.numeric(str_sub(stringr::str_replace_all(seq_meses, "-", ""), 1, 6))
  return(meses)
}


