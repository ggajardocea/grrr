ver <- function(df, var){
  require(magrittr)
  if(c("data.frame") %in% class(df)){
  table(df[,var]) %>% View()
  } else{
    table(df) %>% View()
  }
}
