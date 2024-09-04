library(RQuantLib)
####BS Eq####

rf=0.0445

BSM = function(s, k, v, r, q, dte, type){
  b = r-q
  T = dte/365.25
  d1 = (log(s/k)+(b+(v^2)/2)*T)/(v*sqrt(T))
  d2 = d1 - v*sqrt(T)
  
  if(type == "c"){
    price = s*exp((b-r)*T)*pnorm(d1) - k*exp(-r*T)*pnorm(d2)
  }else{
    price = k*exp(-r*T)*pnorm(-d2) - s*exp((b-r)*T)*pnorm(-d1)
  }
  price
}

####Optimize Function####
error = function(price, s, k, v, r, q, dte, type){
  abs(price - BSM(s, k, v, r, q, dte, type))
}

ImpVol = function(price, s, k, r, q, dte, type){
  IV = optim(par = 1, fn = error, lower = 0, upper = 5, price = price, s = s, k = k, r = r, q = q, dte = dte, type = type, method = "L-BFGS-B")
  IV$par
}


####Data Cleaning####
setwd("C:\\Users\\Charlie/Desktop/R Stuff/Options Pull")


###create trading day calendar

trading = create.calendar(name = "Trading Days", weekdays = c("sunday", "saturday"), holidays = as.Date(c("2023-11-23","2023-12-25")), start.date = as.Date("2023-01-01"), end.date = as.Date("2023-12-31") )

startday = as.Date("2023-10-11")
endday = as.Date("2023-10-12")

dates = bizseq(startday, endday, cal = "Trading Days")

pulls = paste0("Options_Pull_", dates)

td = data.frame()
for (i in 1:length(pulls)){
  td = rbind(td, read.csv(pulls[i]))
}






td = td |>
  select(-Change, -X..Change) |>
  mutate(across(c(Strike:Open.Interest, TQQQ:HVol), ~as.numeric(gsub('[-,%+]', "", .x)))) |>
  mutate(across(Last.Price:Ask, ~ifelse(is.na(.x), 0, .x))) |>
  na.omit() |>
  distinct()

og = td |>
  mutate(Type = ifelse(grepl("P", Contract.Name), "p","c"), S0 = ifelse(grepl("S", Contract.Name), SQQQ, TQQQ),
         Expiry = as_date(substr(Contract.Name, 5, 10)), Price = rowMeans(td[,5:6]), U = ifelse(grepl("S", Contract.Name), "SQQQ", "TQQQ"), ODay = as_date(as.POSIXct.numeric(QuoteTime)),
         t = as.numeric(Expiry - ODay), Implied.Volatility = as.numeric(gsub("%","", Implied.Volatility))/100 ) |>
  filter(ODay > as_date("2023-10-02"))

td = td |>
  mutate(Type = ifelse(grepl("P", Contract.Name), "p","c"), S0 = ifelse(grepl("S", Contract.Name), SQQQ, TQQQ),
         Expiry = as_date(substr(Contract.Name, 5, 10)), Price = rowMeans(td[,5:6]), U = ifelse(grepl("S", Contract.Name), "SQQQ", "TQQQ"), ODay = as_date(as.POSIXct.numeric(QuoteTime)),
         t = as.numeric(Expiry - ODay), Implied.Volatility = as.numeric(gsub("%","", Implied.Volatility))/100 ) |>
  select(Type, Strike, S0, Price, U, t, ODay, QuoteTime, Implied.Volatility)

#####

ivt = td |>
  rowwise() |>
  mutate(IV = ImpVol(Price, S0, Strike, rf, 0, t, Type))

Imp = ivt |>
  ungroup() |>
  filter(IV != 0)

d1o1 = td |>
  filter(QuoteTime == "1696429249")

d1o1 = d1o1 |>
  mutate(Moneyness = ifelse(Type == "P", (Strike)/S0, (S0)/Strike), Price = ifelse(Price == 0, .006, Price))

TC = d1o1 |>
  filter(Type == "c", U == "TQQQ", t < 252, Moneyness > .6 & Moneyness < 2)

TP = d1o1 |>
  filter(Type == "p", U == "TQQQ", t == 30)

####Smile####

smile = plot(TC$Moneyness, TC$Implied.Volatility)


####Fitting Surface####

GTC = acast(TC, t ~ Moneyness, value.var = "Implied.Volatility")

toint = which(is.na(GTC))
coords = cbind(toint%%dim(GTC)[1], toint%/%dim(GTC)[1]+1)


coords[coords[,1] == 0, 2] <- coords[coords[,1] == 0, 2] - 1 
coords[coords[,1] == 0, 1] <- dim(GTC)[1]

for(i in 1:nrow(coords)){
  #get the coordinates of a 10x10 area around the missing value
  x1 <- max(coords[i,1] - 10, 1)
  x2 <- min(coords[i,1] + 10, dim(GTC)[1])
  y1 <- max(coords[i,2] - 10, 1)
  y2 <- min(coords[i,2] + 10, dim(GTC)[2])
  
  #get the moneyness/time to mat combination of the missing value
  x0 <- as.numeric(rownames(GTC)[coords[i,1]])
  y0 <- as.numeric(colnames(GTC)[coords[i,2]])
  
  #get the part of the grid that is used to interpolate and remove all missing values that are present
  interpGrid <- GTC[x1:x2,y1:y2]
  interpGrid <- melt(interpGrid)
  interpGrid <- na.omit(interpGrid)
  interpVal <- interp(x = interpGrid$Var1, y = interpGrid$Var2, z = interpGrid$value,
                      xo = x0, yo = y0,
                      linear = TRUE, extrap = TRUE)$z[1,1]
  
  #if linear interpolation doesnt yield a result, use spline interpolation
  if(is.na(interpVal)){
    interpVal <- interp(x = interpGrid$Var1, y = interpGrid$Var2, z = interpGrid$value,
                        xo = x0, yo = y0,
                        linear = FALSE, extrap = TRUE)$z[1,1]
  }
  
  #if the resulting value is clearly wrong, e.g. negative or way outside the values that are used to interpolate,
  #leave it as NA
  if(interpVal < 0 | interpVal > max(interpGrid$value * 1.5)){
    interpVal <- NA
  }
  
  #replace the value with the result of the interpolation
  GTC[coords[i,1],coords[i,2]] <- interpVal
}


##### Graph Stuff####
xaxx <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=TRUE,
  backgroundcolor='rgb(230, 230,230)',
  title = "Moneyness"
)

yaxx <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=TRUE,
  backgroundcolor='rgb(230, 230,230)',
  title = "Time To Maturity"
)

zaxx <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=TRUE,
  backgroundcolor='rgb(230, 230,230)',
  tickformat = "%",
  title = "Implied Volatility"
)

fig <- plot_ly(x = colnames(GTC), y =  rownames(GTC), z = GTC)
fig <- fig %>% add_surface()
fig <- fig %>% layout(scene = list(xaxis=xaxx, yaxis=yaxx, zaxis = zaxx))
fig <- fig %>% plotly::colorbar(title = "", x = 0.9, y = 0.75, tickformat = "%")
fig



####Looking at data####
og |>
  group_by(ODay) |>
  summarize(count = n())%>%
  ungroup()


Imp |>
  group_by(ODay)|>
  summarize(count = n())%>%
  ungroup()

td |>
  filter(Price == 0) |>
  view()
#####




####Single Contract View####

single = og |>
  filter(Contract.Name == "SQQQ231006C00018500")

ggplot(single, aes(x = S0, y = Price)) +
  geom_point()

y= single[90:128,c(12,17,19)]

plot_ly(single, x = ~QuoteTime, y = ~S0, z = ~Price, type = "scatter3d", mode = "lines" )

####Moneyness View####

og = og |>
  mutate(Moneyness = ifelse(Type == "P", (Strike)/S0, (S0)/Strike), Price = ifelse(Price == 0, .006, Price))


#####Creating data frames for each type####

SP = og |>
  filter(U == "SQQQ" & Type == "p") |>
  select(Moneyness, QuoteTime, Implied.Volatility, U, t) |>
  mutate(QuoteTime = as.numeric(QuoteTime))

SC = og |>
  filter(U == "SQQQ" & Type == "c") |>
  select(Moneyness, QuoteTime, Implied.Volatility, U, t) |>
  mutate(QuoteTime = as.numeric(QuoteTime))

TP = og |>
  filter(U == "TQQQ" & Type == "p") |>
  select(Moneyness, QuoteTime, Implied.Volatility,U, t) |>
  mutate(QuoteTime = as.numeric(QuoteTime))

TC = og |>
  filter(U == "TQQQ" & Type == "c") |>
  select(Moneyness, QuoteTime, Implied.Volatility, U, t) |>
  mutate(QuoteTime = as.numeric(QuoteTime))

y= full_join(SP,TC, by = c("QuoteTime", "t"))

y = y |>
  mutate(dif = abs(Moneyness.x-Moneyness.y)) |>
  filter(dif < .01)


plot1 = plot_ly(x = y$Moneyness.x, y = y$QuoteTime, z = y$Implied.Volatility.x, type = 'scatter3d', mode = 'lines', name = 'SQQQP', line = list(color = 'blue', width = 1))
plot2 = add_trace(plot1, x = y$Moneyness.y, y = y$QuoteTime, z = y$Implied.Volatility.y, type = 'scatter3d', mode = 'lines', name = 'TQQQC', line = list(color = 'red', width = 1))


#test#
difference_left_join(test1,test2, by = "Moneyness", max_dist = .02, distance_col = "abs")
test1 = SP |>
  select(Moneyness, QuoteTime, Implied.Volatility)
test2 = TC |>
  select(Moneyness, QuoteTime, Implied.Volatility)
test1 = test1[47,]
test2 = test2[29,]





##### TQQQ Call & SQQQ Put####

#moneyness bounds#
x = 1.05

comp = og |>
  filter(U == "SQQQ" & Type == "p" | U == "TQQQ" & Type == "c") 
####finding time span of option in question####

contracts = comp |>
  filter(ODay == min(ODay)) |>
  mutate(D = abs(Moneyness - x)) |>
  group_by(Type) |>
  slice(which.min(D)) |>
  filter(t == min(t)) |>
  pull(Contract.Name)

comp1 = comp |>
  filter(Contract.Name %in% contracts) |>
  group_by(Type) |>
  mutate(LogRet = log(Price / lag(Price))) |>
  filter(!is.na(LogRet), ) |>
  slice(3:n()) |>
  mutate(CR = exp(cumsum(LogRet))) |>
  ungroup() 




#check#

comp1 |>
  group_by(QuoteTime) |>
  summarize(Length = n()) |> view()

ggplot(comp1, aes(x = QuoteTime, y = CR, color = Type)) +
  geom_point()

view(comp1)

##### TQQQ Put & SQQQ Call#####

x = 1.05

comp = td |>
  filter(U == "SQQQ" & Type == "C" | U == "TQQQ" & Type == "P") |>
  mutate(Moneyness = ifelse(Type == "P", (Strike)/S0, (S0)/Strike), Price = ifelse(Price == 0, .001, Price))

####finding time span of option in question####

contracts = comp |>
  filter(ODay == min(ODay), DTE == 24) |>
  mutate(D = abs(Moneyness - x)) |>
  group_by(Type) |>
  slice(which.min(D)) |>
  pull(Contract.Name)

comp1 = comp |>
  filter(Contract.Name %in% contracts) |>
  group_by(Type) |>
  mutate(LogRet = log(Price / lag(Price))) |>
  filter(!is.na(LogRet)) |>
  mutate(CR = exp(cumsum(LogRet))) |>
  ungroup()


#check#

comp1 |>
  group_by(QuoteTime) |>
  summarize(Length = n()) |> view()

ggplot(comp1, aes(x = QuoteTime, y = CR, color = Type)) +
  geom_point()
