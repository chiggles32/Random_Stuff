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
preteqwt=Return.portfolio(pret,eqwt)
pretkmlmwt=Return.portfolio(pret,kmlmwt)
##overweight kmlm##

Return.annualized(Return.portfolio(pret,wt,rebalance_on="quarters"))

total=cbind(pretwt,preteqwt,pretkmlmwt)
colnames(total)=c("Current Port","Equal Weight Port","KMLM OverWeight EQ Port")
chart.CumReturns(total,legend.loc="topleft")

###portfolio -kmlm###

ppricenk=pprice[,-21]
wtnk=wt[-21]
wtnk[29]=wtnk[29]+wt[21]

##kmlm weight adjusted to voo##

pretnk=Return.calculate(ppricenk)
pretnkwtnk=Return.portfolio(pretnk,wtnk)

total=cbind(pretwt,preteqwt,pretkmlmwt,pretnkwtnk)
colnames(total)=c("Current Port","Equal Weight Port","KMLM OverWeight EQ Port","Current Port Less KMLM")
chart.CumReturns(total,legend.loc="topleft")


##Testing Distribution Characteristics##

getSymbols("SPY", from="2021-12-02")
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


