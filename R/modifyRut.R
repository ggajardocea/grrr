#' Modifica el rut
#'
#' Sirve para transformar el rut a numérico para mayor velocidad de computo
#'
#' @param x character que tiene el rut completo
#' @param option character que define el output, si es que se quiere obtener el DV o solo el inicio
#'
#' @return character o numeric, dependiendo de las opciones
#' @export
#'
#' @examples
#' modifyRut("0017994104-k", option = "complete")
#' modifyRut("0017994104-k", option = "dv")
#' modifyRut("0017994104-k", option = "rut")
modifyRut <- function(x, option = "rut"){
  # Se encuentra un dash y se define desde donde contar
  x <- stringr::str_replace_all(x, " ", "")
  x <- stringr::str_replace_all(x, "[.]", "")
  dash <- stringr::str_locate(x, "-")[,2]
  end <- ifelse(is.na(dash), 2, 3)

  # Se revisa que función tiene para entregar el output
  if(option == "complete"){
    rut <- toupper(paste0(as.numeric(stringr::str_sub(x, start=1, end=-end)), "-", stringr::str_sub(x, start=-1, end=-1)))
  } else if(option == "rut"){
    rut <- as.numeric(stringr::str_sub(x, start=1, end=-end))
  } else if(option == "dv"){
    rut <- toupper(stringr::str_sub(x, start=-1, end=-1))
  } else{
    print("Ingrese una opción válida")
  }
  return(rut)
}
