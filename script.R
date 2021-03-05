# STAT 571 - Practice of Statistics
# Project 3
# Name: Jana Osea
# Due Date: March 14, 2021


data <- read.csv("data.csv")
View(data)

cherry_df <- data[which(data$species=="cherry"),2:3]
pear_df <- data[which(data$species=="pear"),2:3]


# Method of Moments for Parameter Estimation
M1_cherry <- matrix(apply(cherry_df, 2, sum) / nrow(cherry_df), nrow = 2)
M2_cherry <- matrix(apply(cherry_df*cherry_df, 2, sum) / nrow(cherry_df), nrow = 2)
mu_cherry <- M1_cherry
sigma_cherry <- M2_cherry - M1_cherry^2

M1_pear <- matrix(apply(pear_df, 2, sum) / nrow(pear_df), nrow = 2)
M2_pear <- matrix(apply(pear_df*pear_df, 2, sum) / nrow(pear_df), nrow = 2)
mu_pear <- M1_pear
sigma_pear <- M2_pear - M1_pear^2




