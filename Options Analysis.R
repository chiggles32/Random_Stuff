##Option Chain Analysis##
library(quantmod)
library(tidyverse)
library(readxl)

T = read_excel(file.choose(), sheet = "TQQQ Options")
ETF = read_excel(file.choose(), sheet = "ETF Price")
S = read_excel(file.choose(), sheet = "SQQQ Options")

o = bind_rows(T,S)

##Cleaning##

o = o |>
  mutate("Percent Change" = gsub("^-$", "0", `Percent Change`), Volume = gsub("^-$", "0", Volume))

##Need to convert all data types properly##

##Tidying##

o = o |>
  mutate(ETF = ifelse(grepl("TQQQ",Contract),"TQQQ","SQQQ"))

0 = o |> 
  inner_join(ETF, by = c("Date", "ETF")) 
         