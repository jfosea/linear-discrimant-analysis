# STAT 571 - Practice of Statistics
# Project 3
# Name: Jana Osea
# Due Date: March 14, 2021

library(mvtnorm)
library(ggplot2)
library(gridExtra)

# import data and separate by species
data <- read.csv("data/data1.csv")
names(data)[1] <- "species"
cherry_df <- data[which(data$species=="cherry"),2:3]
pear_df <- data[which(data$species=="pear"),2:3]


# method of moments for parameter estimation
source("functions/estimate params.R")
param_cherry <- calculate_parameters(cherry_df)
param_pear <- calculate_parameters(pear_df)
Sigma <- (param_cherry$Sigma + param_pear$Sigma) / 2


# density calculations
source("functions/density functions.R")
data$density_cherry <- mapply(dens_cherry, data$width, data$length)
data$density_pear <- mapply(dens_pear, data$width, data$length)
data$density_cherry1 <- mapply(dens_cherry1, data$width, data$length)
data$density_pear1 <- mapply(dens_pear1, data$width, data$length)

# lambda calculations
source("functions/classification functions.R")
data$lambda <- mapply(calculate_lambda, data$density_cherry, data$density_pear)
data$classification <- mapply(classify, data$lambda)
data$lambda1 <- mapply(calculate_lambda, data$density_cherry1, data$density_pear1)
data$classification1 <- mapply(classify, data$lambda1)


# (6) classify new values
source("functions/predict functions.R")
u_ <- classify_new(32,82)
v_ <- classify_new(38,52)
w_ <- classify_new(40,76)

u1_ <- classify_new_second(32,82)
v1_ <- classify_new_second(38,52)
w1_ <- classify_new_second(40,76)

# (7) visualization
source("functions/plotting functions.R")
plot <- plot_overall(Sigma, param_cherry, param_pear, data)
plot <- plot_data("Plot of Sample", data)
plot_linear <- plot_classification(title="Classification Using Equal Covariance", data, assumption="linear")
plot_quadratic <- plot_classification(title="Classification Using Unequal Covariance", data, assumption="quadratic")
plot_both <- plot_classification(title="Classification Boundaries", data, assumption="both")
plot_none <- plot_classification(title="First Rule Classification", data, assumption="none")
# grid_linear <- predict_boundary(linear=TRUE)
# grid_quadratic <- predict_boundary(linear=FALSE)
# plot_pred_boundary_linear <- plot_boundary(title="Predicted Decision Boundary of Linear", grid_linear)
# plot_pred_boundary_quad <- plot_boundary(title="Predicted Decision Boundary of Quadratic", grid_quadratic)

plot
plot_linear 
plot_quadratic
plot_both
plot_pred_boundary_linear
plot_pred_boundary_quad




