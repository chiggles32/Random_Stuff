library(tidyverse)
library(jsonlite)
library(lubridate)
library(quantmod)
library(PerformanceAnalytics)
library(tibbletime)
library(purrr)
library(ggridges)
library(gtsummary)
library(ggthemes)
library(TTR)
library(randomForest)
library(caret)
theme_set(theme_economist())
library(party)

getSymbols("SPY",from = "2000-01-01", to = "2020-01-01")

bbi = ifelse(spy$SPY.Adjusted > bbands[,3],1,ifelse(spy$SPY.Adjusted < bbands[,1], -1,0))

rolled_sd = rollify(sd,5)

bbands = BBands(spy$SPY.Adjusted, n = 22,,2)

spy = spy |>
  mutate(Return = Delt(SPY.Adjusted), CO = wilderSum((sign(Return)*SPY.Volume*SPY.Close),n=5)/runMean(SPY.Volume*SPY.Close,n=5),
          rsd = rolled_sd(Return), MODCO = CO/rsd, BB = bbi)

spytrain = spy[1:2500,7:11]
spytest = spy[2501:3000,7:11]

spytrain[,1] = lag(spytrain[,1])
spytest[,1] = lag(spytest[,1])

spytrain = na.omit(spytrain)
spytest = na.omit(spytest)

rf = randomForest(Return ~., data = spytrain, ntree = 300, mtry = 4)

p1 <- predict(rf, spytest)
confusionMatrix(p1,spytest$Return)
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf,
           sort = T,
           n.var = 5,
           main = "Top 10 - Variable Importance")

importance(rf)

mytree = ctree(Return~., spytrain, controls = ctree_control(mincriterion=0.9, minsplit=50))
plot(mytree,type="simple")

tab <- table(predict(mytree), spytrain$Return)
1-sum(diag(tab))/sum(tab)
