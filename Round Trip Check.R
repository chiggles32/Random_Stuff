##Roundtrips
library(tidyverse)
library(jsonlite)
library(lubridate)
library(quantmod)
library(PerformanceAnalytics)
library(tibbletime)
library(purrr)
library(ggridges)
library(gtsummary)
library(TTR)

etf = read.table(file.choose(),sep = ",", header = F)

colnames(etf) = c("D/T", "Open", "High", "Low", "Close", "Volume")

etf = etf |>
  separate("D/T", c("Date","Time"), sep = " ", remove = F) |>
  filter(hms(Time) >= hms("09:30:00") & hms(Time) < hms("16:00:00"))

ret_time = format(seq(as.POSIXct("09:30:00", format = "%H:%M:%S"), by = "30 min", length.out = 14), format = "%H:%M:%S")

##intitial try with using close prices only

intervals = etf |>
  filter(hms(Time) %in% hms(ret_time)) |>
  select(Date, Time, Close) 

one_day = etf |> 
  filter(Date == "2005-01-03") |>
  select(Time:Low) |>
  mutate(across(where(is.numeric), ~ round(.,2)))

mat = as.matrix(one_day[,2:4])

table(mat)

one_day2 = etf |> 
  filter(Date == "2005-02-03") |>
  select(Time:Low) |>
  mutate(across(where(is.numeric), ~ round(.,2)))

mat = as.matrix(one_day2[,2:4])

table(mat)
plot(table(mat))
