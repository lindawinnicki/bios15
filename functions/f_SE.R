#creating the SE function
SE <- function(data){
  n = sum(!is.na(data), na.rm = T)
  StE = sqrt(var(data, na.rm = T)/n)
  return(StE)
}
getwd()