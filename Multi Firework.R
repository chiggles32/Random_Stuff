library(tidyverse)

x11(width = 11, height = 10)
Sys.sleep(3)
par(mar = c(0, 0, 0, 0))
plot.new()
usr <- par("usr") # Get the user coordinates of the plot region
rect(usr[1], usr[3], usr[2], usr[4], col = hsv(0,0,0,1), border = NA)
# firework brightness decay function

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
  
  color = runif(1)
  
  x <<- tail(x,1)
  y <<- tail(y,1)
  
  w = cbind(x, y, frame,color, b)
  dimnames(w) = NULL
  w
}

missile = missile_initial()

swarm_size = 300

particle_initial = function(n){
  #output a vector of size 5
  # x = .5*usr[2]
  # y = .5*usr[4]
  velo = runif(1,-.01,.01)
  xv = 1.2 * velo * cos(n * ((2*pi)/swarm_size))
  yv = velo * sin(n * ((2*pi)/swarm_size))
  frame = 1
  c(x,y,xv,yv,frame)
}
particles = t(sapply(1:300, particle_initial))

particles_update = function(x){
  x[,1] = x[,1] + x[,3]
  x[,2] = x[,2] + x[,4]
  x[,3] = x[,3] * .98
  x[,4] = x[,4] - .0003
  x[,5] =x[,5] + 1
  x
}

color = rnorm(300,missile[length(missile[,1]),3], .03)
color = ifelse(color>=1,.99, ifelse(color<=0,0,color))

particle_in_time = list()
particle_in_time[[1]] = particles
particle_in_time[[1]] = cbind(particle_in_time[[1]], color, 1)

for (i in 2:50) {
  particle_in_time[[i]] = particles_update(particle_in_time[[i-1]])
}

drop_velo = function(x) {
  dimnames(x) = NULL
  x[,5] = x[,5] + nrow(missile)
  x[,c(-3,-4)]
}

particles_in_time = lapply(particles_in_time, drop_velo)

m_l = lapply(1:nrow(missile), function(x) t(as.matrix(missile[x,])))

graph_data = c(m_l, particle_in_time)

colorize = function(x) {
  color = hsv(x[,4],1,x[,5],1)
  x = as.data.frame(x)
  x = x[,c(-4,-5)]
  x$color = color
  x
}

graph_data = lapply(graph_data, colorize)

graph_data2 = do.call(rbind, graph_data)

graph_data1 = do.call(rbind, graph_data)
graph_data1[,3] = graph_data1[,3] + 11

particles_display = function(x){
  rect(usr[1], usr[3], usr[2], usr[4], col = hsv(0,0,0,.3), border = NA)
  points(x[,1],x[,2], pch = 8, col = x[,4], cex = 1.1)
  Sys.sleep(.05)
  dev.flush()
}

names(graph_data1) = c("xp", "yp", "frame", "color")
names(graph_data2) = c("xp", "yp", "frame", "color")

all_graph = rbind(graph_data1, graph_data2)

w = all_graph |>
  group_by(frame) |>
  group_split()

w = lapply(w, as.data.frame)

x11(width = 11, height = 10)
Sys.sleep(3)
par(mar = c(0, 0, 0, 0))
plot.new()
usr <- par("usr") # Get the user coordinates of the plot region
rect(usr[1], usr[3], usr[2], usr[4], col = hsv(0,0,0,1), border = NA)

lapply(w, particles_display)

# for (i in 1:length(graph_data)) {
#   
# }


