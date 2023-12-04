



barplot_density <- function(df, colors, xlabel, xaxt="s", yaxt="s", order_vec=NULL, filter_threshold=NULL){
  
  require(prettyGraphs)
  
  colnames(df) =c("group", "value")
  
  #function to rescale the density (for the color scale) between 0 and 12
  scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
  
  #function to plot one single bar
  rect_alpha_density <- function(values, alpha_levels=500, ymax, ymin) {
    
    density_plot <- density(values)
    alpha_seq <- (scale_values(density_plot$y))
    
    # Draw a single rectangle with alpha gradient
    rect_alpha_gradient <- function(xleft, ybottom, xright, ytop, alpha_levels=1000, alpha_seq) {
      
      n_rescale<- length(alpha_seq)
      # Scaling the density vector to the new length
      scaled_density_y <- approx(1:n_rescale, alpha_seq, n = alpha_levels)$y
      
      alphas <- scaled_density_y
      n <- length(alphas)
      x <- seq(xleft, xright, length.out = n + 1)
      
      for (j in 1:n) {
        rect(x[j], ybottom, x[j + 1], ytop, col = prettyGraphs::add.alpha(colors[i], alphas[j]), border = NA, density = -1, angle = 45)
      }
    }
    
    rect_alpha_gradient(range(density_plot$x)[1],ymin, range(density_plot$x)[2], ymax, alpha_seq=alpha_seq)
    
  }
  
  data=df

  #be sure to have factors as groupsß
  data$group <- as.factor(data$group)
  data_levels <- levels(data$group)
  
  #eventually reorder the classes
  if(!is.null(order_vec)){data_levels<-data_levels[order_vec]}
  
  #filter threshold
  if(!is.null(filter_threshold)){
    factor_counts <- length(which(table(data$group) > filter_threshold))
    } else {factor_counts <- length(levels(data$group))}
  
  
  #create the canvas
  plot(1, type = "n", xlim = range(data$value), 
       ylim = c(0, factor_counts-0.2), xlab = "", ylab = "", xaxt=xaxt, yaxt=yaxt)
  
  layer_plot=0.4 #counter to increment the height of each bar
  
  #looping for each data level
  
  for(i in 1:length(data_levels)){
    
    if(length(data[data$group==data_levels[i],]$value)>2){
    rect_alpha_density(values=data[data$group==data_levels[i],]$value, ymin=layer_plot-0.4, ymax=layer_plot+0.4)
    
      if(yaxt=="s"){axis(side = 2, at = layer_plot, labels = data_levels[i], las=2)}
    
      # abline(h=layer_plot-0.4, lty=1, col="lightgray")
      # abline(h=layer_plot+0.4, lty=1, col="lightgray")
      
    layer_plot = layer_plot+1}
    
  }
  
  mtext(side = 1,  text=xlabel, line=3, cex=0.7)

  
}



darken_colors <- function(colors, percentage) {
  # Convert hex colors to RGB format
  rgb_vals <- col2rgb(colors)
  
  # Calculate the darkened RGB values
  darkened_rgb <- rgb(t(apply(rgb_vals, 2, function(x) x * (1 - percentage))), maxColorValue = 255)
  
  return(darkened_rgb)
}

lighten_colors <- function(colors, percentage) {
  # Convert hex colors to RGB format
  rgb_vals <- col2rgb(colors)
  
  # Calculate the lightened RGB values
  lightened_rgb <- rgb(t(apply(rgb_vals, 2, function(x) x + (255 - x) * percentage)), maxColorValue = 255)
  
  return(lightened_rgb)
}
