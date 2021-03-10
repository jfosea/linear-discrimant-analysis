classify_new <- function(x,y) {
  density_cherry <- dens_cherry(x,y)
  density_pear <-dens_pear(x,y)
  lambda <- calculate_lambda(density_cherry, density_pear)
  classification <- classify(lambda)
  return(list("density_cherry" = density_cherry,
              "density_pear" = density_pear,
              "lambda" = lambda,
              "classification" = classification))
}
