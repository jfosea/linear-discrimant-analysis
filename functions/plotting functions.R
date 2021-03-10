plot_overall <- function(Sigma, param_cherry, param_pear, data) {
  Sigma_inv <- solve(Sigma)
  p = (param_cherry$mu - param_pear$mu)[1]
  q = (param_cherry$mu - param_pear$mu)[2]
  m = Sigma_inv[1,1]
  n = Sigma_inv[1,2]
  o = Sigma_inv[2,2]
  c = 0.5*((t(param_cherry$mu) %*% Sigma_inv %*% param_cherry$mu)  - (t(param_pear$mu) %*% Sigma_inv %*% param_pear$mu) )
  
  constant = c/(p*n+q*o)
  slope = -(p*m+q*n)/(p*n+q*o)
  constant
  slope
  scatter_plot <-  ggplot(data,aes(x=width, y=length, color=classification, shape=species)) + 
    geom_point(size=5) + 
    scale_color_manual(values = c('#999999','#E69F00')) + 
    theme(legend.position="bottom", legend.justification=c(0,1)) +
    geom_abline(intercept = constant, slope = slope, color="red")
  scatter_plot
  
  xdensity <-  ggplot(data, aes(x=width, fill=species)) + 
    geom_density(alpha=.5) + 
    scale_fill_manual(values = c('#999999','#E69F00')) + 
    theme(legend.position = "none")
  xdensity
  ydensity <- ggplot(data, aes(y=length, fill=species)) + 
    geom_density(alpha=.5) + 
    scale_fill_manual(values = c('#999999','#E69F00')) + 
    theme(legend.position = "none")
  ydensity
  
  blank_plot <- ggplot()+geom_blank(aes(1,1))+
    theme(plot.background = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks = element_blank()
    )
  
  
  return(grid.arrange(xdensity, blank_plot, scatter_plot, ydensity, 
               ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)))
}


plot_classification <- function(title="Title", data, equal=TRUE) {
  if(equal) {
    return(ggplot(data,aes(x=width, y=length, color=classification, shape=species)) + 
             geom_point(size=5) + 
             scale_color_manual(values = c('#999999','#E69F00')) + 
             theme(legend.position="bottom", legend.justification=c(0,1)) + 
             ggtitle(title) +
             xlab("Width (mm)") + 
             ylab("Length (mm)"))
    } 
  if(!equal) {
    return(ggplot(data,aes(x=width, y=length, color=classification1, shape=species)) + 
           geom_point(size=5) + 
           scale_color_manual(values = c('#999999','#E69F00')) + 
           theme(legend.position="bottom", legend.justification=c(0,1)) + 
           ggtitle(title) +
           xlab("Width (mm)") + 
           ylab("Length (mm)"))
  }
}


classify_new1 <- function(x,y) {
  density_cherry <- dens_cherry(x,y)
  density_pear <-dens_pear(x,y)
  lambda <- calculate_lambda(density_cherry, density_pear)
  classification <- classify(lambda)
  return(classification)
}

classify_new2 <- function(x,y) {
  density_cherry <- dens_cherry1(x,y)
  density_pear <-dens_pear1(x,y)
  lambda <- calculate_lambda(density_cherry, density_pear)
  classification <- classify(lambda)
  return(classification)
}

predict_boundary <- function(linear=TRUE) {
  Width <- c()
  Length <- c()
  predicted_species <- c()
  if(linear) {
    for (i in seq(50,110)) {
      Width <- append(Width, seq(25, 55, 0.1))
      Length <- append(Length,rep(i, 10*(55-24)-9))
      predicted_species <- append(predicted_species,mapply(classify_new1, seq(25, 55, 0.1), i))
    }
  }
  if(!linear) {
    for (i in seq(50,110)) {
      Width <- append(Width, seq(25, 55, 0.1))
      Length <- append(Length,rep(i, 10*(55-24)-9))
      predicted_species <- append(predicted_species,mapply(classify_new2, seq(25, 55, 0.1), i))
    }
  }
  v1 <- data.frame(Width,Length,predicted_species)
  return(v1)
}

plot_boundary <- function(title="Title", data) {
  return(ggplot(data,aes(x=Width, y=Length, color=predicted_species)) + 
           geom_point(size=2.5, shape="square") + 
           scale_color_manual(values = c("#E69F00", "#999999")) + 
           theme(legend.position="bottom", legend.justification=c(0,1)) +
           ggtitle(title) +
           xlab("Width (mm)") + 
           ylab("Length (mm)"))
}
