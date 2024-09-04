library(PerformanceAnalytics)
library(quantmod)
library(tidyverse)
library(lubridate)


getSymbols("^GSPC",from = "1980-01-01", to = "2015-01-01",periodicity = "daily")
gspc = to.monthly(GSPC,indexAt = "end")
spr = Return.calculate(gspc[,6])

#S&P 500 for comparison

factors = read.csv(file.choose())
ind = make_datetime(factors[,1], factors[,2], factors[,3])
factors = factors[,-1:-3]

fret = as.xts(factors,ind)

#fret is the xts of all returns

all_fact_port = Return.portfolio(fret)

comp = merge(all_fact_port,spr)

colnames(comp) = c("All Factors Portfolio", "S&P 500")

chart.CumReturns(comp,legend.loc = 'topleft')

AdjustedSharpeRatio(comp)

chart.Correlation(merge(spr,fret[,1:10]))

#RR plot great to visualize ineffectiveness of individual strategy

chart.RiskReturnScatter(fret)


#Sharpe Ratio of different number of factors in port

optimal_factors = function(retmat,stepsize,trials){
  nc = ncol(retmat)
  steps = seq(1,nc,stepsize)
  stepnum = length(steps)
  ind = index(retmat)
  sharpe_mat = matrix(,trials,stepnum)

  for (i in 1:stepnum){
	samples = replicate(trials,retmat[,sample(nc,steps[i])])

	for (j in 1:trials){
		sharpe_mat[j,i] = SharpeRatio.annualized(Return.portfolio(as.xts(samples[,,j],ind)))
		}
	}
  sharpe_mat<<-sharpe_mat
  
}
	
optimal_factors(fret,5,30)
plot(colMeans(sharpe_mat),typ='l')


#Rerturn of different number of factors in port

optimal_factors_ret = function(retmat,stepsize,trials){
  nc = ncol(retmat)
  steps = seq(1,nc,stepsize)
  stepnum = length(steps)
  ind = index(retmat)
  ret_mat = matrix(,trials,stepnum)

  for (i in 1:stepnum){
	samples = replicate(trials,retmat[,sample(nc,steps[i])])

	for (j in 1:trials){
		ret_mat[j,i] = Return.annualized(Return.portfolio(as.xts(samples[,,j],ind)))
		}
	}
  ret_mat<<-ret_mat
  
}


optimal_factors_ret(fret,3,250)
plot(colMeans(ret_mat),typ='l')

##optimal factors vol based

optimal_factors_vol = function(retmat,stepsize,trials){
  nc = ncol(retmat)
  steps = seq(1,nc,stepsize)
  stepnum = length(steps)
  ind = index(retmat)
  vol_mat = matrix(,trials,stepnum)

  for (i in 1:stepnum){
	samples = replicate(trials,retmat[,sample(nc,steps[i])])

	for (j in 1:trials){
		vol_mat[j,i] = StdDev.annualized(Return.portfolio(as.xts(samples[,,j],ind)))
		}
	}
  vol_mat<<-vol_mat
  
}


optimal_factors_vol(fret,3,250)
plot(colMeans(vol_mat),typ='l')


#creating a new portfolio based on highest trailing 12 months sharpe ratio, no look ahead bias.

 rollsharpe = function (retmat){
   nc = ncol(retmat)
   nr = nrow(retmat)
 
   smat = matrix(,nr,nc)
 
   for (i in 1:nc){
 smat[,i] = apply.rolling(retmat[,i], width = 12, trim = F, gap = 12, by = 1, FUN = SharpeRatio.annualized)
   }
 
 smat <<- as.xts(smat,ind)
 }
 
rollsharpe(fret)

#annualized returns close to maxing out when 25 factors are present 

get25factors = function(mat){
  nr = nrow(mat)
  selected = matrix(,nr,25)
  for (i in 1:nr){
	topval = head(sort(as.vector(mat[i,]),decreasing = T),n = 25)
	selected[i,] = match(topval,mat[i,])
	}
  selected <<- selected
}

smat = smat[-1:-11]
get25factors(smat)
selected = selected[-409,]

newfret = fret[-1:-12,]

selfact = function(retmat,factsel){
  nr = nrow(retmat)
  selectret = matrix(,nr,25)
  for (i in 1:nr){
	selectret[i,] = retmat[i,factsel[i,]]
	}
  selectret <<- as.xts(selectret,index(retmat))
}

selfact(newfret,selected)

selport = Return.portfolio(selectret,reblance_on = 'months')
comp = merge(comp,selport)
colnames(comp) = c("All Factor Portfolio", "S&P 500", "Lagged Sharpe Portfolio")
chart.CumReturns(comp,legend.loc = 'topleft')
	
comp = comp[-1:-12,]

boxplot(comp)

table.Stats(comp)
VolatilitySkewness(comp)



