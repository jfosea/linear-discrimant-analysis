calculate_lambda <- function(x,y) return(x/y)
classify <- function(lambda) {
  if (lambda > 1) return("cherry") 
  else if (lambda < 1) return("pear")
  else return("undetermined")
} 
