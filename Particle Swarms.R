
missile_initial = function(n){
  #output a vector of size 5
  x = .5*usr[2]
  y = 0*usr[4]
  velo = runif(1,.01,.02)
  xv = 0
  yv = velo
  c(x,y,xv,yv)
}
missile_update = function(x){
  x[,1] = x[,1] + x[,3]
  x[,2] = x[,2] + x[,4]
  x[,3] = x[,3] + runif(1, -.001, .001)
  x[,4] = x[,4] - .00001
  x
}
missile_display = function(x, color){
  rect(usr[1], usr[3], usr[2], usr[4], col = hsv(0,0,0,.3), border = NA)
  points(x[,1],x[,2], pch = 8, col = color, cex = 1.1)
}


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

firework(200, 100)
