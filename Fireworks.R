

generate_sigmoid_vector <- function(n, steepness = 10, midpoint = 0.8) {
  x <- seq(0, 1, length.out = n)  # Create a normalized sequence from 0 to 1
  sigmoid_vector <- 1 / (1 + exp(steepness * (x - midpoint)))  # Logistic decay
  sigmoid_vector
}

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
  
  color <<- hsv(runif(1),1,b,1)
  color1 <<- tail(color,1)
  
  cbind(x,y,b)
}

missile_display = function(x){
  
  
  
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


swarm_size = 300

particles_initial = function(n){
  #output a vector of size 5
  # x = .5*usr[2]
  # y = .5*usr[4]
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
  
  missile = missile_initial()
  x <<- tail(missile,1)[1]
  y <<- tail(missile,1)[2]
  particles = t(sapply(1:size, particles_initial))
  
  z = runif(size,-.03,.03)
  color = runif(1) + z
  color = ifelse(color>=1,.99, ifelse(color<=0,0,color))
  decay = generate_sigmoid_vector(iter,25,.7)
  
  missile_display(missile)
  
  for (i in 1:iter){
    particles = particles_update(particles)
    particles_display(particles, hsv(color,1,decay[i],1))
    Sys.sleep(.03)
    dev.flush()
  }
  
}


x11(width = 11, height = 10)
Sys.sleep(3)
par(mar = c(0, 0, 0, 0))
plot.new()
usr <- par("usr") # Get the user coordinates of the plot region
rect(usr[1], usr[3], usr[2], usr[4], col = hsv(0,0,0,1), border = NA)
for (i in 1:4){
  firework(450,50)
}

