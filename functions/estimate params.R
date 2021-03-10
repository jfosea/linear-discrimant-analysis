calculate_parameters <- function(df) {
  mu1 <- matrix(apply(df, 2, mean),nrow = 2)
  mu2 <- matrix(apply(df*df, 2, mean), nrow = 2)
  muxy <- mean(apply(df,1,prod))
  muxmuy <- prod(mu1)
  
  sigma_x_y <- mu2 - mu1^2
  sigma_xy <- muxy - muxmuy
  Sigma <- matrix(c(sigma_x_y[1,1], sigma_xy,
                    sigma_xy, sigma_x_y[2,1]),
                  ncol = 2, nrow = 2)
  return(list("mu" = mu1, "Sigma"= Sigma))
  
}