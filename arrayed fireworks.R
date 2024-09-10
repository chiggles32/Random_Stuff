x11(width = 11, height = 10)
par(mar = c(0, 0, 0, 0))
plot.new()
usr <- par("usr") # Get the user coordinates of the plot region
rect(usr[1], usr[3], usr[2], usr[4], col = hsv(0,0,0,1), border = NA)

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
  
  decay = generate_sigmoid_vector(50,15,.5)
  
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

fireworks = function(x) {
  spacer = cumsum(round(rnorm(x, 40, 10)))
 
  fdata = list()
  fdata[[1]] = generate_firework()
  
  for (i in 2:x){
    w = generate_firework()
    w[,3] = w[,3] + spacer[i]
    fdata[[i]] = w
  }

  graph_data = do.call(rbind, fdata)
  
  graph_data[,4] = ifelse(graph_data[,4]>=1,.99, ifelse(graph_data[,4]<=0,0,graph_data[,4]))
  
  listed_frames = lapply(split(graph_data, graph_data[,3]), function(x) matrix(x, ncol = 5))
  
  listed_frames
}

display_fireworks = function(x) {
  rect(usr[1], usr[3], usr[2], usr[4], col = hsv(0,0,0,.3), border = NA)
  points(x[,1],x[,2], pch = 16, col = hsv(x[,4], 1, x[,5],1), cex = .5)
  Sys.sleep(.05)
  dev.flush()
}

lapply(fireworks(10), display_fireworks)



