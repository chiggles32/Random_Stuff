   SMIF Asset Portfolio

library(quantmod)
library(PerformanceAnalytics)
library(jsonlite)

tickers=c("AFK","CGW","CIBR","COWZ","DBA","DSTL","EEMV","EMXC","EWY",
"FM","FTEC","GBTC","IAK","ETHE","IBB","ICLN","ILF","IQIN","ITA","JETS","KMLM","MLPX","ONEY","PSCI","QQQM","SOXX",
"SRLN","SRVR","VDC","VOO","VPU","VTV","XLF","XLV","XLY")

##subs=c(AFK[,6],CGW[,6],CIBR[,6],COWZ[,6],DBA[,6],DSTL[,6],EEMV[,6],EMXC[,6],EWY[,6],#
##FM[,6],FTEC[,6],GBTC[,6],IAK[,6],ETHE[,6],IBB[,6],ICLN[,6],ILF[,6],IQIN[,6],ITA[,6],JETS[,6],KMLM[,6],MLPX[,6],ONEY[,6],PSCI[,6],QQQM[,6],SOXX[,6],#
##SRLN[,6],SRVR[,6],VDC[,6],VOO[,6],VPU[,6],VTV[,6],XLF[,6],XLV[,6],XLY[,6])#

owt=c(.02,.04,.05,.04,.02,.04,.02,.01,.01,.01,.02,0,0,.01,0,.03,.02,.05,0,.03,.05,.05,.04,.04,.03,.03,
.04,.02,.05,0,.05,.05,.04,.05,.03)
numshares=c(500,612,725,500,652,632,199,382,127,307,145,77,183,564,126,487,300,608,
157,1019,383,774,262,273,148,45,588,438,175,89,108,235,490,220,136)

length(tickers)==length(numshares)

lp=getQuote(tickers)[,2]

position = lp*numshares

wt=position/sum(position)

eqwt=rep((1/35),35) 

##Earliest Date For Full Returns: 2020-12-02##
##  days  ##
##yyyy-mm-dd##

kmlmwt=c(rep((.9/34),20),.1,rep((.9/34),14))

smifport=cbind(tickers,wt)

getSymbols(tickers,from="2020-12-02")

pprice=cbind(AFK[,6],CGW[,6],CIBR[,6],COWZ[,6],DBA[,6],DSTL[,6],EEMV[,6],EMXC[,6],EWY[,6],
FM[,6],FTEC[,6],GBTC[,6],IAK[,6],ETHE[,6],IBB[,6],ICLN[,6],ILF[,6],IQIN[,6],ITA[,6],JETS[,6],KMLM[,6],MLPX[,6],ONEY[,6],PSCI[,6],QQQM[,6],SOXX[,6],
SRLN[,6],SRVR[,6],VDC[,6],VOO[,6],VPU[,6],VTV[,6],XLF[,6],XLV[,6],XLY[,6])

pret=Return.calculate(pprice) 

pretwt=Return.portfolio(pret,wt)



##Testing Distribution Characteristics##

getSymbols("SPY", from="2020-12-02")
spy=SPY[,6]
spy=Return.calculate(spy)
spy=spy[-1]

getSymbols("SPY", from="2000-01-01")
SPY2000=SPY[,6]
SPY2000=Return.calculate(SPY2000)
SPY2000=SPY2000[-1]

getSymbols("SPY", from="2005-01-01")
SPY2005=SPY[,6]
SPY2005=Return.calculate(SPY2005)
SPY2005=SPY2005[-1]

getSymbols("SPY", from="2010-01-01")
SPY2010=SPY[,6]
SPY2010=Return.calculate(SPY2010)
SPY2010=SPY2010[-1]

getSymbols("SPY", from="2015-01-01")
SPY2015=SPY[,6]
SPY2015=Return.calculate(SPY2015)
SPY2015=SPY2015[-1]

getSymbols("SPY", from="2020-01-01")
SPY2020=SPY[,6]
SPY2020=Return.calculate(SPY2020)
SPY2020=SPY2020[-1]

trial2=window(SPY2000,start="2001-01-01",end="2003-01-01")
ks.test(trial2,trial)



ks.test(SPY,pretwt)
ks.test(spy,pretwt)
ks.test(spy,SPY)

##spy same time horizon as smif portfolio##
##SPY from 2000##

##Analyzing Distributions##

distr=function (allreturns,horizon) 
{
ret=as.numeric(allreturns)
t=length(ret)
z=t-horizon
indexnum=c(1:z)
mdr=c()
kss=c()
sds=c()



for (i in indexnum){
	mdr[i]=mean(ret[i:(i-1+horizon)])
	kss[i]=kurtosis(ret[i:(i-1+horizon)])
	sds[i]=sd(ret[i:(i-1+horizon)])

}

meandailyret=mean(mdr)
meandailyretsd=sd(mdr)
meankurt=mean(kss)
meankurtsd=sd(kss)
meanrtsd=mean(sds)
meanrtsdsd=sd(sds)

results=cbind(meandailyret,meandailyretsd,meankurt,meankurtsd,meanrtsd,meanrtsdsd)

results

}


#return evolution#

disev=function (allreturns,stepsize) 
{

ret=as.numeric(allreturns)
t=length(ret)
stepnum=floor(z/stepsize)
steps=(1:stepnum)*stepsize
storage=matrix(0,stepnum,6)


for (j in 1:stepnum){

z=t-steps[j]
indexnum=c(1:z)

mdr=c()
kss=c()
sds=c()




for (i in indexnum){
	mdr[i]=mean(ret[i:(i-1+steps[j])])
	kss[i]=kurtosis(ret[i:(i-1+steps[j])])
	sds[i]=sd(ret[i:(i-1+steps[j])])

}

meandailyret=mean(mdr)
meandailyretsd=sd(mdr)
meankurt=mean(kss)
meankurtsd=sd(kss)
meanrtsd=mean(sds)
meanrtsdsd=sd(sds)

results=cbind(meandailyret,meandailyretsd,meankurt,meankurtsd,meanrtsd,meanrtsdsd)

storage[j,]=results

}
evolution<<-storage

}


##bootstrapped beta##

resample=function (ret,period,k) 
{
##ret in the form of bm, portfolio##

l=length(ret[,1])
bsdata=matrix(0,k,2)



for ( i in 1:k){

	ind=floor(runif(period,1,l))
	bsret=ret[ind,]
	fit=lm(bsret[,1]~bsret[,2])
	bsdata[i,]=fit$coefficients
}

bsdata<<-bsdata

}


##portfolio mc graph##

function (ret,period,iter) 
{
q=floor(runif(iter,1,650))
par(new=F)
for (i in 1:iter){

bsret=sample(ret,period,replace=T)
bsret=bsret+1
cumr=cumprod(bsret)
plot(cumr,typ='l',ylim=c(.75,1.5),col=q[i])
par(new=T)

}

}












#bootstrapping returns#
#k=iterations#

bootxts=function (dataframe,k) 
{
resample_index1 <- sample(nrow(dataframe),size=nrow(dataframe),replace=TRUE)
resample1 <- dataframe[resample_index1,]
resample_storage <- array(rep(0,length=nrow(dataframe)*ncol(dataframe)*k),c(nrow(dataframe),ncol(dataframe),k))
for (i in 1:k) {
      resample_index <- sample(nrow(dataframe),size=nrow(dataframe),replace=TRUE) 
      resample_storage[,,i] <- dataframe[resample_index,]
    }

##attach indices##

for (i in 1:k){
resample_storage[,,i]=as.xts(resample_storage[,,i],order.by=index(dataframe))
}

bootstrapped_returns<<-resample_storage
}



#plotting random returns of portfolios#
#need to make matrix into xts object#
function(weights,returns)



bootxts=function (dataframe,k) 
{
resample_index1 <- sample(nrow(dataframe),size=nrow(dataframe),replace=TRUE)
resample1 <- dataframe[resample_index1,]
resample_storage <- xts(array(rep(0,length=nrow(dataframe)*ncol(dataframe)*k),c(nrow(dataframe),ncol(dataframe),k)),order.by=index(dataframe))
for (i in 1:k) {
      resample_index <- sample(nrow(dataframe),size=nrow(dataframe),replace=TRUE) 
      resample_storage[,,i] <- dataframe[resample_index,]
    }


bootstrapped_returns<<-resample_storage
}


##multi portfolio mc graph##

function (ret1,ret2,period,iter) 
{

time=1:period
ret1=as.vector(ret1)
ret2=as.vector(ret2)
par(new=F)
for (i in 1:iter){

bsret1=sample(ret1,period,replace=T)
bsret1=bsret1+1
cumr1=cumprod(bsret1)

bsret2=sample(ret2,period,replace=T)
bsret2=bsret2+1
cumr2=cumprod(bsret2)

ports=cbind(cumr1,cumr2)

plot(time,cumr1,typ='l',ylim=c(.5,2),col=4)
points(time,cumr2,typ='l',col=3)
par(new=T)


}


}



