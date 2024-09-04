#Ridge Plots

library(tidyverse)
library(ggridges)
library(Performancenalytics)
library(quantmod)

getSymbols("SPY" , from = "1993-01-01", periodicity = "daily")

spy = Delt(SPY[,6])[-1]

ridges_fun = function(ret, horizon){

ret = as.numeric(ret)
t = length(ret)
num_steps = ceiling(t/horizon)
indice = sort(rep(1:num_steps, horizon))[1:t]

retvec = as_tibble(cbind(indice,ret))
retvec = retvec |> 
  mutate(indice = as.factor(indice))

#colnames(retvec) = c("Labels","Returns")

retdf <<- retvec

} 

ridges_fun(spy,500)


ggplot(retdf, aes( x = ret, y = indice, fill = indice)) +
geom_density_ridges() +
theme_ridges() +
theme(legend.position = "none")+
xlim(-.08,.08)

getSymbols("AAPL" , from = "1993-01-01", periodicity = "daily")

aapl = Delt(AAPL[,6])[-1]

ridges_fun(aapl,252)

