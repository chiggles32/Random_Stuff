

x11(width = 11, height = 10)
par(mar = c(0, 0, 0, 0), bg = "black")
plot.new()
usr <- par("usr") # Get the user coordinates of the plot region

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
  dxv = rnorm(n, drift, .003)
  xv = 0
  yv = rep(velo, n)
  decel = -.00025 * 0:(n-1)
  yv = yv + decel
  x = x + cumsum(dxv)
  y = cumsum(y + yv) + decel
  b = runif(n)
  frame = 1:n
  
  color <<- runif(1)
  
  x <<- tail(x,1)
  y <<- tail(y,1)
  
  w = cbind(x, y, frame, color, b)
  dimnames(w) = NULL
  w
}

particle_initial = function(n){
  swarm_size = 300
  #output a vector of size 5
  # x = .5*usr[2]
  # y = .5*usr[4]
  velo = runif(1,-.01,.01)
  xv = 1.2 * velo * cos(n * ((2*pi)/swarm_size))
  yv = velo * sin(n * ((2*pi)/swarm_size))
  frame = 1
  b = 1
  c(x,y,xv,yv,frame,color,b)
}

particles_update = function(x){
  x[,1] = x[,1] + x[,3]
  x[,2] = x[,2] + x[,4]
  x[,3] = x[,3] * .98
  x[,4] = x[,4] - .0003
  x[,5] = x[,5] + 1
  x
}

drop_velo = function(x) {
  
  x[,5] = x[,5] + nrow(missile)
  x[,c(-3,-4)]
  
}

generate_firework = function() {
  
  swarm_size = 300
  
  missile <<- missile_initial()
  
  particles = t(sapply(1:300, particle_initial))
  
  particles[,6] = particles[,6] + rnorm(300,0,.1)
  particles[,6] =  ifelse(particles[,6]>=1,.99, ifelse(particles[,6]<=0,0,particles[,6]))
  
  
  decay = generate_sigmoid_vector(50,13,.6)
  
  particles_in_time = list()
  particles_in_time[[1]] = particles
  
  for (i in 2:50) {
    particles_in_time[[i]] = particles_update(particles_in_time[[i-1]])
    particles_in_time[[i]][,7] = decay[i]
  }
  
  particles_in_time = lapply(particles_in_time, drop_velo)
  
  m_l = lapply(1:nrow(missile), function(x) t(as.matrix(missile[x,])))
  
  graph_data = c(m_l, particles_in_time)
  
  do.call(rbind, graph_data)
  
}

display_fireworks = function(x) {
  rect(usr[1], usr[3], usr[2], usr[4], col = hsv(0,0,0,.3), border = NA)
  points(x[,1],x[,2], pch = 16, col = hsv(x[,4], 1, x[,5],1), cex = .15)
  Sys.sleep(.033)
  dev.flush()
}




initial_firework = generate_firework()
counter = 0
while (TRUE) {

  counter = counter + 1
  display_fireworks(matrix(initial_firework[initial_firework[,3]==counter,], ncol = 5))
  
  random_spacer = round(runif(1, 18,50))
  
  if (counter >= random_spacer){
    next_firework = generate_firework()
    next_firework[,3] = next_firework[,3] + random_spacer
    initial_firework = rbind(initial_firework, next_firework)
    
    initial_firework = initial_firework[-1:-counter,]
    initial_firework[,3] = initial_firework[,3] - counter
    counter = 0
  }
  
  
  
}


