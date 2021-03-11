theme_ed <- theme(
  text=element_text(size=11),
  legend.position = "bottom",
  panel.background = element_rect(fill = NA),
  panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey85"),
  panel.grid.major = element_line(color = "grey95", size = 0.2),
  panel.grid.minor = element_line(color = "grey95", size = 0.2),
  legend.key = element_blank())

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
  scatter_plot <-  ggplot(data,aes(x=width, y=length, color=classification, shape=species)) + 
    geom_point(size=5) + 
    scale_color_manual(values = c('#999999','#E69F00')) + 
    theme(legend.position="bottom", legend.justification=c(0,1)) +
    geom_abline(intercept = constant, slope = slope, color="red") + theme_ed
  scatter_plot
  
  xdensity <-  ggplot(data, aes(x=width, fill=species)) + 
    geom_density(alpha=.5) + 
    scale_fill_manual(values = c('#999999','#E69F00')) + 
    theme(legend.position = "none")  + theme_ed
  xdensity
  ydensity <- ggplot(data, aes(y=length, fill=species)) + 
    geom_density(alpha=.5) + 
    scale_fill_manual(values = c('#999999','#E69F00')) + 
    theme(legend.position = "none") + theme_ed
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


plot_classification <- function(title="Title", data, assumption=c("linear", "quadratic", "both", "none")) {

  quad_boundary <- function(x) {
    sigA_inv = solve(param_cherry$Sigma)
    sigB_inv = solve(param_pear$Sigma)
    m = (sigA_inv - sigB_inv)[1,1]
    n = (sigA_inv - sigB_inv)[1,2]
    o = (sigA_inv - sigB_inv)[2,2]
    
    p = as.numeric((-2*(sigA_inv %*% param_cherry$mu - sigB_inv %*% param_pear$mu))[1,1])
    q = as.numeric((-2*(sigA_inv %*% param_cherry$mu - sigB_inv %*% param_pear$mu))[2,1])
    
    f1 = 2*(log(sqrt(det(param_cherry$Sigma)/det(param_pear$Sigma))))
    f2 = as.numeric(t(param_cherry$mu) %*% sigA_inv %*% param_cherry$mu) - as.numeric(t(param_pear$mu) %*% sigB_inv %*% param_pear$mu)
    c = f1 + f2
    
    A = o
    B = (2*n*x+q)
    C = (m*x^2 + x*p + c)
    return((-B-sqrt(B^2-4*A*C))/(2*A))
  }
  
  linear_boundary <- function(x) {
    Sigma_inv <- solve(Sigma)
    p = (param_cherry$mu - param_pear$mu)[1]
    q = (param_cherry$mu - param_pear$mu)[2]
    m = Sigma_inv[1,1]
    n = Sigma_inv[1,2]
    o = Sigma_inv[2,2]
    c = 0.5*((t(param_cherry$mu) %*% Sigma_inv %*% param_cherry$mu)  - (t(param_pear$mu) %*% Sigma_inv %*% param_pear$mu) )
    
    constant = as.numeric(c/(p*n+q*o))
    slope = as.numeric(-(p*m+q*n)/(p*n+q*o))
    return(constant+ slope*x)
  }
  
  linear_plot <- ggplot(data,aes(x=width, y=length, color=classification, shape=species)) + 
    geom_point(size=5) + 
    scale_color_manual(values = c('#E69F00','#999999')) + 
    theme(legend.position="bottom", legend.justification=c(0,1)) + 
    ggtitle(title) +
    xlab("Width (mm)") + 
    ylab("Length (mm)")+ theme_ed +  ylim(50,110)
  
  quad_plot <- ggplot(data,aes(x=width, y=length, color=classification1, shape=species)) + 
    geom_point(size=5) + 
    scale_color_manual(values = c('#E69F00','#999999')) + 
    theme(legend.position="bottom", legend.justification=c(0,1)) + 
    ggtitle(title) +
    xlab("Width (mm)") + 
    ylab("Length (mm)")  + theme_ed +  ylim(50,110)
  
  if(assumption=="linear") {
    return(linear_plot + stat_function(fun=linear_boundary, col="red"))
    } 
  if(assumption=="quadratic") {
    return(quad_plot + stat_function(fun=quad_boundary, col="blue"))
  }
  if(assumption=="both") {
    return(linear_plot + stat_function(fun=quad_boundary, col="blue") +  stat_function(fun=linear_boundary, col="red"))
  }
  if(assumption=="none") {
    return(linear_plot)
  }
}

plot_data <- function(title="Title", data) {
  return(ggplot(data,aes(x=width, y=length, color=species, shape=species)) + 
    geom_point(size=5) + 
    scale_color_manual(values = c('#E69F00','#999999')) + 
    theme(legend.position="bottom", legend.justification=c(0,1)) + 
    ggtitle(title) +
    xlab("Width (mm)") + 
    ylab("Length (mm)")+ theme_ed +  ylim(50,110))
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
           ylab("Length (mm)")  + theme_ed)
}
