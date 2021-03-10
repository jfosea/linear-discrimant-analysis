dens_cherry <- function(width,length) {
  return(dmvnorm(c(width,length), mean = param_cherry$mu, Sigma))
}

dens_pear <- function(width,length) {
  return(dmvnorm(c(width,length), mean = param_pear$mu, Sigma))
}
dens_cherry1 <- function(width,length) {
  return(dmvnorm(c(width,length), mean = param_cherry$mu, param_cherry$Sigma))
}

dens_pear1 <- function(width,length) {
  return(dmvnorm(c(width,length), mean = param_pear$mu, param_pear$Sigma))
}
