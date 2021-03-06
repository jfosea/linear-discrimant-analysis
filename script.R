# STAT 571 - Practice of Statistics
# Project 3
# Name: Jana Osea
# Due Date: March 14, 2021

# import data and separate by species
data <- read.csv("data.csv")
cherry_df <- data[which(data$species=="cherry"),2:3]
pear_df <- data[which(data$species=="pear"),2:3]

# Method of Moments for Parameter Estimation
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
  print(list("mu" = mu1, "Sigma"= Sigma))
  
}

param_cherry <- calculate_parameters(cherry_df)
param_pear <- calculate_parameters(pear_df)
Sigma <- (param_cherry$Sigma + param_pear$Sigma) / 2


# lambda calculations

