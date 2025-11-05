CV <- function(data){
  n = sum(!is.na(data), na.rm = T)
  CVa = sd(data) / mean(data)
  return(CVa)
}