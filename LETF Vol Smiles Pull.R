
data <- data.frame(
  Index = c( "SPY","QQQ","IYG","XLK","GDX","DIA","XLF","IWM","EEM"),
  Leveraged_ETF = c("SPXL","TQQQ", "FAS", "TECL", "NUGT", "DDM","FAS", "TNA","EET"),
  Inverse_Leveraged_ETF = c("SPXS","SQQQ","FAZ","TECS","DUST", "DXD", "FAZ","TZA","EEV" )
)

tickers = as.vector(c(data$Index,data$Leveraged_ETF,data$Inverse_Leveraged_ETF))
 
library(tidyverse)
library(rvest)
library(lubridate)
library(bizdays)
library(quantmod)
library(PerformanceAnalytics)
library(httr)

####Checking the fit####

getSymbols(tickers, from = "2022-01-01")

etfs = cbind(SPY[,6], QQQ[,6], IYG[,6], XLK[,6], GDX[,6], DIA[,6], XLF[,6],IWM[,6],EEM[,6],SPXL[,6],TQQQ[,6],FAS[,6],TECL[,6],NUGT[,6],DDM[,6],FAS[,6],TNA[,6],EET[,6],SPXS[,6],SQQQ[,6],FAZ[,6],TECS[,6],DUST[,6],DXD[,6], FAZ[,6],TZA[,6],EEV[,6])
  
etfret = Return.calculate(etfs, method = "log")

spy = cbind(etfret[,c(1,10,19)]) |>
  na.omit()
colnames(spy) = sub(".Adjusted", "", colnames(spy))

qqq = cbind(etfret[,c(2,11,20)])|>
  na.omit()
colnames(qqq) = sub(".Adjusted", "", colnames(qqq))

iyg = cbind(etfret[,c(3,12,21)])|>
  na.omit()
colnames(iyg) = sub(".Adjusted", "", colnames(iyg))

xlk = cbind(etfret[,c(4,13,22)])|>
  na.omit()
colnames(xlk) = sub(".Adjusted", "", colnames(xlk))

gdx = cbind(etfret[,c(5,14,23)])|>
  na.omit()
colnames(gdx) = sub(".Adjusted", "", colnames(gdx))

dia = cbind(etfret[,c(6,15,24)])|>
  na.omit()
colnames(dia) = sub(".Adjusted", "", colnames(dia))

xlf = cbind(etfret[,c(7,16,25)])|>
  na.omit()
colnames(xlf) = sub(".Adjusted", "", colnames(xlf))

iwm = cbind(etfret[,c(8,17,26)])|>
  na.omit()
colnames(iwm) = sub(".Adjusted", "", colnames(iwm))

eem = cbind(etfret[,c(9,18,27)])|>
  na.omit()
colnames(eem) = sub(".Adjusted", "", colnames(eem))

x = list(spy, qqq, iyg, xlk, gdx, dia, xlf, iwm, eem)


y=list()
for (i in 1:length(x)){
  y[[i]] = lm(
    as.formula(paste(colnames(x[[i]])[2], "~",
                     paste(colnames(x[[i]])[1], collapse = "+"),
                     sep = ""
    )),
    data=x[[i]]
  )
  y[[i+10]] = lm(
    as.formula(paste(colnames(x[[i]])[3], "~",
                     paste(colnames(x[[i]])[1], collapse = "+"),
                     sep = ""
    )),
    data=x[[i]]
  )
  y[[i+20]] = lm(
    as.formula(paste(colnames(x[[i]])[2], "~",
                     paste(colnames(x[[i]])[3], collapse = "+"),
                     sep = ""
    )),
    data=x[[i]]
  )
}
p=list()
for (i in 1:length(x)){
  
  message(i)
  
  p[[i]] = local({
    i = i
    p1 = ggplot(x[[i]], aes(x = x[[i]][,2], y = x[[i]][,1])) +
    geom_point() +
    geom_smooth(method = "lm", formula = y~x) +
    labs(title = "Index vs LETF Daily Returns", x = colnames(x[[i]])[1], y = colnames(x[[i]])[2])
    print(p1)
  })
  
  p[[i+9]] = local({
    i = i
    p1 = ggplot(x[[i]], aes(x = x[[i]][,3], y = x[[i]][,1])) +
      geom_point() +
      geom_smooth(method = "lm", formula = y~x) +
      labs(title = "Index vs LETF Daily Returns", x = colnames(x[[i]])[1], y = colnames(x[[i]])[3])
    print(p1)
  })
    
  
  p[[i+18]] = local({
    i = i
    p1 = ggplot(x[[i]], aes(x = x[[i]][,2], y = x[[i]][,3])) +
      geom_point() +
      geom_smooth(method = "lm", formula = y~x) +
      labs(title = "LETF vs LETF Daily Returns", x = colnames(x[[i]])[2], y = colnames(x[[i]])[3])
    print(p1)
  })
  
  
}
p[[1]]
########




####Option chains####

data = data |>
  group_by(Index) |>
  group_split()

data = data[-3]

dates = list()
for (i in 1:length(data)){
  date1 = read_html(paste0("https://finance.yahoo.com/quote/",data[[i]][1],"/options?p=",data[[i]][1]))
  x = date1 |>
    html_nodes("option") |>
    html_text() |>
    as.Date(format = "%B %d, %Y") |>
    as.POSIXct() |>
    as.numeric()
  
  date2 = read_html(paste0("https://finance.yahoo.com/quote/",data[[i]][2],"/options?p=",data[[i]][2]))
  y = date2 |>
    html_nodes("option") |>
    html_text() |>
    as.Date(format = "%B %d, %Y") |>
    as.POSIXct() |>
    as.numeric()
  
  date3 = read_html(paste0("https://finance.yahoo.com/quote/",data[[i]][3],"/options?p=",data[[i]][3]))
  z = date3 |>
    html_nodes("option") |>
    html_text() |>
    as.Date(format = "%B %d, %Y") |>
    as.POSIXct() |>
    as.numeric()
  
  dates[[i]] = intersect(intersect(x,y),z)
  
}
####Getting options data####
all_links = list()
for (i in 1:length(dates)){
  for (j in 1:length(dates[[i]])){
    all_links = c(all_links, new_entry = paste0("https://finance.yahoo.com/quote/",data[[i]][1],"/options?p=",data[[i]][1],"&date=",dates[[i]][j]))
    all_links = c(all_links, new_entry = paste0("https://finance.yahoo.com/quote/",data[[i]][2],"/options?p=",data[[i]][2],"&date=",dates[[i]][j]))
    all_links = c(all_links, new_entry = paste0("https://finance.yahoo.com/quote/",data[[i]][3],"/options?p=",data[[i]][3],"&date=",dates[[i]][j]))
    
  }
  
}


test_group = c(colnames(dia), colnames(eem))
test_group = sub(".Adjusted", "", test_group)

aldf = do.call(rbind, all_links)

aldf = aldf[grepl(paste(test_group, collapse = "|"), aldf, ignore.case = TRUE)]

####Setting dates to be pulled####


webpull = function(link){
  od = data.frame()
  webpage = read_html(link)
  tbl_node = html_nodes(webpage, "table")
  od = rbind(od, as.data.frame(html_table(tbl_node[1], fill = TRUE)))
  od = rbind(od, as.data.frame(html_table(tbl_node[2], fill = TRUE)))
  od
  Sys.sleep(15)
}

#####Testing Links####
###too many requests####
####split data

test_link = function(link){
  response = GET(link)
  sc = status_code(response)
  sc
}

check = lapply(all_links,test_link)

test = list(aldf[1],aldf[2],aldf[3],aldf[4],aldf[5],aldf[6],aldf[7],aldf[8],aldf[9],aldf[10],aldf[11],aldf[12])

options_data = lapply(test, webpull)

options_data = list()
for (i in 1:length(aldf)){
  options_data = c(options_data, new_entry = webpull(aldf[i]))
}


op = "C:\\Users\\Charlie/Desktop/R Stuff/Options Pull"
opf = file.path(op, paste("Options_Pull", Sys.Date(), sep = "_"))

if(file.exists(opf)){
  td = read.csv(opf)
  td = rbind(td,td1)
} else {
  td = td1
}

write.csv(td, file = opf, row.names = FALSE)




