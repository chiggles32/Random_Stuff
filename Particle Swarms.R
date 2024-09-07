par(mar = c(0, 0, 0, 0))
plot.new()
usr <- par("usr")

missile_initial = function(i = 75){
  drift = runif(1,-.005, .005)
  n = rnorm(1, i, i/20)
  #output a vector of size 3
  x = .5*usr[2]
  y = 0*usr[4]
  velo = runif(1,.015,.025)
  dxv = rnorm(n, drift, .001)
  xv = 0
  yv = rep(velo, n)
  decel = -.00025 * 0:(n-1)
  yv = yv + decel
  x = x + cumsum(dxv)
  y = cumsum(y + yv) + decel
  b = runif(n)
  # cutoff = round(n * .3)
  # bn = sample(c(0,0,1), cutoff, replace = TRUE)
  # b[((n-cutoff)+1):n] = bn
  cbind(x,y,b)
}

missile_display = function(x){
  
  x11()
  Sys.sleep(.5)
  par(mar = c(0, 0, 0, 0))
  plot.new()
  usr <- par("usr") # Get the user coordinates of the plot region
  rect(usr[1], usr[3], usr[2], usr[4], col = hsv(0,0,0,1), border = NA)
  
  color <<- hsv(runif(1),1,x[,3],1)
  
  for (i in 1:nrow(x)){
    
    if(i>30){points(x[i-2,1],x[i-2,2], pch = 8, col = hsv(0,0,0,1), cex = 1.1)
    rect(usr[1], usr[3], usr[2], usr[4], col = hsv(0,0,0,.3), border = NA)
    points(x[i,1],x[i,2], pch = 8, col = color[i], cex = 1.1)
    } else {
      rect(usr[1], usr[3], usr[2], usr[4], col = hsv(0,0,0,.3), border = NA)
      points(x[i,1],x[i,2], pch = 8, col = color[i], cex = 1.1)}
      
    Sys.sleep(.03)
    dev.flush()
  }
  
}

missile_function = function(){
  missile_display(missile_initial())
}

missile_function()

swarm_size = 300

particles_initial = function(n){
  #output a vector of size 5
  x = .5*usr[2]
  y = .5*usr[4]
  velo = runif(1,-.01,.01)
  xv = 1.2 * velo * cos(n * ((2*pi)/swarm_size))
  yv = velo * sin(n * ((2*pi)/swarm_size))
  c(x,y,xv,yv)
}
particles_update = function(x){
  x[,1] = x[,1] + x[,3]
  x[,2] = x[,2] + x[,4]
  x[,3] = x[,3] * .98
  x[,4] = x[,4] - .0003
  x
}
particles_display = function(x, color){
  rect(usr[1], usr[3], usr[2], usr[4], col = hsv(0,0,0,.3), border = NA)
  points(x[,1],x[,2], pch = 8, col = color, cex = 1.1)
}

firework = function(size, iter){
  x11()
  Sys.sleep(3)
  par(mar = c(0, 0, 0, 0))
  plot.new()
  usr <- par("usr") # Get the user coordinates of the plot region
  rect(usr[1], usr[3], usr[2], usr[4], col = hsv(0,0,0,1), border = NA)
  
  x = t(sapply(1:size, particles_initial))
  
  y = runif(size,-.03,.03)
  color = runif(1) + y
  color = ifelse(color>=1,.99, ifelse(color<=0,0,color))
  color = hsv(color,1,1,1)
  
  for (i in 1:iter){
    x = particles_update(x)
    particles_display(x, color)
    Sys.sleep(.03)
    dev.flush()
  }
  
}

firework(200, 20)




# 
# 
# convert_hex_to_argb_int <- function(hex_color) {
#   # Remove the leading "#" if present
#   hex_color <- gsub("#", "", hex_color)
#   
#   # Extract RGBA components (Red, Green, Blue, Alpha)
#   r <- as.integer(paste0("0x", substr(hex_color, 1, 2)))  # Red
#   g <- as.integer(paste0("0x", substr(hex_color, 3, 4)))  # Green
#   b <- as.integer(paste0("0x", substr(hex_color, 5, 6)))  # Blue
#   a <- as.integer(paste0("0x", substr(hex_color, 7, 8)))  # Alpha
#   
#   # Pack into a single integer (ARGB format)
#   argb_int <- bitwShiftL(a, 24) + bitwShiftL(r, 16) + bitwShiftL(g, 8) + b
#   
#   return(argb_int)
# }
# 
# # Example usage:
# hex_color <- "#00000033"
# color_int <- convert_hex_to_argb_int(hex_color)
# 
# color_int  # Numeric integer value representing the ARGB color

fire = function(n=200, swarm_size = 300){
  x = missile_initial()
  start_position = tail(x,1)
  particles = t(sapply(1:size, particles_initial))
}
