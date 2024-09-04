library(tidyverse)
library(RQuantLib)
library(plotly)


rates = seq(from = -.5, to = .5, by = .025)
times = seq(from = 1/252, to = 1, by = 1/252)
vols = seq(from = 0, to = 1, by = .05)

test = EuropeanOptionArrays("c", 1,1,0,rates,times,.15)

td = expand.grid(x = as.numeric(rownames(test$value)), y = as.numeric(colnames(test$value)))
td$z = c(test$value)

colnames(td) = c("Rates", "Time", "Price")

plot_ly(td, x = ~Time, y = ~Rates, z = ~Price)

testc = EuropeanOption("c", 1,1,0,rates,1,.45)


####
GBSM.Price = function(spot, strike, volatility, riskfree, yield, dte, type){
  
  t = dte/365.25
  
  d1 = (1 / (volatility * sqrt(t))) * (log(spot/strike) + (riskfree - yield + (.5 * volatility^2)) * t)
  d2 = d1 - (volatility * sqrt(t))
  
  price = ifelse(type == "call",
                 (spot * exp(-yield * t) * pnorm(d1)) - (strike * exp(-riskfree * t) * pnorm(d2)),
                 (strike * exp(-riskfree * t) * pnorm(-d2)) - (spot * exp(-yield * t) * pnorm(-d1))
  )
  price
}

GBSM.Greeks = function(spot, strike, volatility, riskfree, yield, dte, type){
  
  t = dte/365.25
  
  d1 = (1 / (volatility * sqrt(t))) * (log(spot/strike) + (riskfree - yield + (.5 * volatility^2)) * t)
  d2 = d1 - (volatility * sqrt(t))
  
  price = ifelse(type == "call",
                 (spot * exp(-yield * t) * pnorm(d1)) - (strike * exp(-riskfree * t) * pnorm(d2)),
                 (strike * exp(-riskfree * t) * pnorm(-d2)) - (spot * exp(-yield * t) * pnorm(-d1))
  )
  
  delta = ifelse(type == "call",
                 exp(-yield*t) * pnorm(d1),
                 exp(-yield*t) * (pnorm(d1)-1)
  )
  
  gama = (dnorm(d1) * exp(-yield*t)) / (spot * volatility * sqrt(t))
  
  theta = ifelse(type == "call",
                 ((-spot * dnorm(d1) * volatility * exp(-yield * t)) / (2 *sqrt(t))) + (yield * spot * pnorm(d1) * exp(-yield*t)) - (riskfree * strike * exp(-riskfree * t) * pnorm(d2)),
                 ((-spot * dnorm(d1) * volatility * exp(-yield * t)) / (2 *sqrt(t))) - (yield * spot * pnorm(-d1) * exp(-yield*t)) + (riskfree * strike * exp(-riskfree * t) * pnorm(-d2))
  )
  vega = spot * sqrt(t) * dnorm(d1) * exp(-yield * t)
  
  rho = ifelse(type == "call",
               strike * t * exp(-riskfree * t) * pnorm(d2),
               -strike * t * exp(-riskfree * t) * pnorm(-d2)
  )
  results = cbind(price, delta, gama, theta, vega, rho)
  
  results
  
  
}

GV = Vectorize(GBSM.Price)


maturity = as.list(seq(from = 1, to = 504, by = 10))

rates

comp.fun = function(x){
  cd = expan
}




####monte carlo the ending value

##can develop more accurate generators in the future
#x = call on index, y = put on inverse

x = replicate(2000000, rnorm(252,0,.03))
y = x*-1

x1 = apply(x+1,2,cumprod)
y1 = apply(y+1,2,cumprod)

x2 = apply(x1, 1, mean)
y2 = apply(y1, 1, mean)

x3 = x2-1
y3 = 1-y2

z = data.frame(1:length(x3),x3,y3)


colnames(z) = c("Days", "Index Call", "Inverse Put")

z1 = pivot_longer(z,cols = 2:3)

ggplot(z1, aes(x = Days, y = value, col = name))+
  geom_line()

###Creating a comp function
library(tidyverse)



comp.fun = function(ndays, dailyvol, rfpa, nsim, leverage){
  
  rfpd = rfpa/252
  
  IndexRet = replicate(nsim, rnorm(ndays, rfpd, dailyvol))
  LevRet = IndexRet * leverage
  ILevRet = LevRet * -1
  
  IVal = apply(IndexRet + 1, 2, cumprod)
  LVal = apply(LevRet + 1, 2, cumprod)
  ILVal = apply(ILevRet + 1, 2, cumprod)
  
  M.IV = apply(IVal, 1, mean)
  M.LV = apply(LVal, 1, mean)
  M.ILV = apply(ILVal, 1, mean)
  
  C.Intr.IV = M.IV - 1 
  C.Intr.LV = M.LV - 1
  C.Intr.ILV = 1 - M.ILV
  
  P.Intr.IV = 1 - M.IV 
  P.Intr.LV = 1 - M.LV
  P.Intr.ILV = M.ILV - 1
  
  ##Enforcing Bounds
  
  C.Intr.IV[C.Intr.IV < 0 ] = 0
  C.Intr.LV[C.Intr.LV < 0 ] = 0
  C.Intr.ILV[C.Intr.ILV < 0 ] = 0
  
  P.Intr.IV[P.Intr.IV < 0 ] = 0
  P.Intr.LV[P.Intr.LV < 0 ] = 0
  P.Intr.ILV[P.Intr.ILV < 0 ] = 0
  
  C.D.IV = C.Intr.IV * exp(-rfpa * (ndays/252))
  C.D.LV = C.Intr.LV * exp(-rfpa * (ndays/252))
  C.D.ILV = C.Intr.ILV * exp(-rfpa * (ndays/252))
  
  P.D.IV = P.Intr.IV * exp(-rfpa * (ndays/252))
  P.D.LV = P.Intr.LV * exp(-rfpa * (ndays/252))
  P.D.ILV = P.Intr.ILV * exp(-rfpa * (ndays/252))
  
  dfd = data.frame(1:ndays, C.D.IV, C.D.LV, C.D.ILV, P.D.IV, P.D.LV, P.D.ILV)
  colnames(dfd) = c("Trading Days", "Index Calls", "LETF Calls", "ILETF Puts", "Index Puts", "LETF Puts", "ILETF Calls")
  
  price_ratios = data.frame(1:ndays)
  price_ratios$C.Index_L = dfd$`LETF Calls` / dfd$`Index Calls` 
  price_ratios$C.Index_IL =  dfd$`ILETF Puts` / dfd$`Index Calls` 
  price_ratios$C.Index_LIL = dfd$`LETF Calls` / dfd$`ILETF Puts`
  price_ratios$P.Index_L = dfd$`LETF Puts` / dfd$`Index Puts` 
  price_ratios$P.Index_IL =  dfd$`ILETF Calls` / dfd$`Index Puts` 
  price_ratios$P.Index_LIL = dfd$`LETF Puts` / dfd$`ILETF Calls`
  
  
  dfd = pivot_longer(dfd, cols = 2:7)
  
  colnames(dfd) = c("Trading Days", "Underlying", "Relative Price")
  
  plot1 = ggplot(dfd, aes(x = `Trading Days`, y = `Relative Price`, col = Underlying))+
    geom_line()
  
  y = list(plot1, price_ratios)
  
  y
}
