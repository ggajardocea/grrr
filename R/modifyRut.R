modifyRut <- function(number, option = "complete"){
  number <- str_replace_all(number, "[.]", "")
  dash <- str_detect(number, "-")
  if(!dash){
    end <- 2
  } else if(dash){
    end <- 3
  }

  if(option == "complete"){
    rut <- toupper(paste0(str_sub(number, start=1, end=-end), "-", str_sub(number, start=-1, end=-1)))
  } else if(option == "rut"){
    rut <- as.numeric(str_sub(number, start=1, end=-end))
  } else if(option == "dv"){
    rut <- toupper(str_sub(number, start=-1, end=-1))
  } else{
    print("Ingrese una opción válida")
  }
}
