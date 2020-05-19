
# idMonth -----------------------------------------------------------------
idMonth <- function(x = 201912, months = 0){
  require(lubridate)
  # Opción si es fecha
  if(class(x) %in% c("Date", "POSIXct", "POSIXt")){
    warning("Identificado como fecha, se trunca")
    x_char <- as.character(x)
    x_mod <- as.numeric(stringr::str_sub(stringr::str_replace_all(lubridate::ymd(x_char) + lubridate::months(months), "-", ""), 1, 6))

  } else if(class(x) %in% c("numeric", "integer", "character")){
    x_char <- as.character(x)
    if(nchar(x_char) == 6){
      x_mod <- as.numeric(stringr::str_sub(stringr::str_replace_all(lubridate::ymd(paste0(x_char, "01")) + lubridate::months(months), "-", ""), 1, 6))
      return(x_mod)
      if(unique(x_mod) %in% NA){
        print("Usar formato YYYYMM")
      }
    } else if(nchar(x_char) != 6){
      warning("Se tomaron solo los primeros 6 digitos")
      x_cut <- str_sub(x, 1, 6)
      x_mod <- as.numeric(stringr::str_sub(stringr::str_replace_all(lubridate::ymd(paste0(x_cut, "01")) + lubridate::months(months), "-", ""), 1, 6))
      return(x_mod)
      if(unique(x_mod) %in% NA){
        print("Usar formato YYYYMM")
      }

    }
  }
}

