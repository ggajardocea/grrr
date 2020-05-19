verDuplicados <- function(df, variable = "rut", show = TRUE){
  colnames(df)[colnames(df) == variable] <- "Revision"
  df_show <- df[df$Revision %in% df$Revision[duplicated(df$Revision)], ]
  colnames(df_show)[colnames(df_show) == "Revision"] <- variable
  if(show){
    View(df_show)}
  else{df_show}
}
