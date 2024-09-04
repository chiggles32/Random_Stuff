

#  ------------------------------------------------------------------ R Test ------------------------------------------------------------------ #


# --- INSTRUCTIONS: 
# You are encouraged to use all available resources to complete the below questions. 
# Please load-in the provided dataset and perform all data manipulations directly in R. Document your work with comments as needed.  
# The final output/answer should either be assigned to a variable as indicated or readily available to print to the console.
# Bonus points for using dplyr and regular expressions.
# Disclaimer: all data is fictitious 

##Session Info
##R version 4.3.0 (2023-04-21 ucrt)
##Platform: x86_64-w64-mingw32/x64 (64-bit)
##Running under: Windows 10 x64 (build 19045)

library(tidyverse)

test_data = read.csv(file.choose())

# Exercises ---------------------------------------------------------------------

# 1) What are the total and average invested amounts for each investment Type?

TA.type = test_data |>
  group_by(Investment.Type) |>
  summarize("Total Invested Amount" = sum(Invested), 
            "Average Invested Amount" = mean(Invested))

# 2) Create a dataframe with the following for each Geography: a) top 5 largest invested amounts, and b) corresponding fund names. 
# Combine "Eastern Europe" and "Western Europe" into one line item: "Europe". Combine "Latin America" and "North America" into one line item: "America".

Geo.df = test_data |>
  mutate(Geography = case_when(grepl("America", Geography) ~ "America",
                               grepl("Europe", Geography) ~ "Europe",
                               TRUE ~ Geography)) |>
  group_by(Geography) |>
  arrange(desc(Invested)) |>
  slice_head(n = 5) |>
  select(Geography, Invested, Fund.Name)

# 3) Create a dataframe with average returns by Strategy for Type A and Type B funds with an investment Date occurring in the years 1990-2010. 

##Left in investment type as I was unsure if you wanted that in the data frame or not
AR.df = test_data |>
  filter(Investment.Type == "Type A" | Investment.Type == "Type B", 
         year(as.Date(Investment.Date, format = "%m/%d/%Y")) > 1989 & year(as.Date(Investment.Date, format = "%m/%d/%Y")) < 2011) |>
  group_by(Investment.Type, Strategy) |>
  summarize("Average Return" = mean(Returns))
  
##By strategy only, no groups for Investment.Type
AR1.df = test_data |>
  filter(Investment.Type == "Type A" | Investment.Type == "Type B", 
         year(as.Date(Investment.Date, format = "%m/%d/%Y")) > 1989 & year(as.Date(Investment.Date, format = "%m/%d/%Y")) < 2011) |>
  group_by(Strategy) |>
  summarize("Average Return" = mean(Returns))

# 4) What percentage of the total invested amount (across all investments) do the top 10 largest investments into NFTs, Crypto, and B2B strategies account for?

x = sum(test_data$Invested)

##Taking top 10 from each. if the top 10 irrespective of the strategy are necessary remove the group_by() function

##Gives the percentage in terms of each individual investment across all investments
percent.df = test_data |>
  filter(Strategy %in% c("NFTs", "B2B", "Crypto")) |>
  group_by(Strategy) |>
  arrange(desc(Invested)) |>
  slice_head(n = 10) |>
  mutate("Total Invested" = x,
         "% of Total" = Invested / `Total Invested`) |>
  select(Fund.Name, Strategy, `% of Total`)

##Gives the percentage in terms of total of the top 10 from each
percent1.df = test_data |>
  filter(Strategy %in% c("NFTs", "B2B", "Crypto")) |>
  group_by(Strategy) |>
  arrange(desc(Invested)) |>
  slice_head(n = 10) |>
  summarize("Sum of Investments" = sum(Invested)) |>
  mutate("% of Total Investments" = `Sum of Investments` / x) |>
  select(-`Sum of Investments`)
  
p = percent1.df |>
  pull(`% of Total Investments`) |>
  sum()

# 5) Create a dataframe with the average invested amount and median returns by year of investment Date for Type C funds with strategies of Shortterm or Longterm.

AM.year = test_data |>
  filter(Investment.Type == "Type C", 
         Strategy %in% c("Longterm", "Shorterm")) |>
  mutate(Investment.Year = year(as.Date(Investment.Date, format = "%m/%d/%Y"))) |>
  group_by(Investment.Year) |>
  summarize("Average Invested" = mean(Invested),
            "Median Returns" = median(Returns)) |>
  arrange(Investment.Year)

# 6) Create a dataframe of all funds with a hyphen "-" in their name and with invested amounts of less than or equal to $20 million. Add a column with the words "Active" or "Liquidated" to indicate whether they are active or non-active based on the field "Active".  

H.df = test_data |>
  filter(grepl("-", Fund.Name),
         Invested <= 20000000) |>
  mutate("A/L" = if_else(Active, "Active", "Liquidated"))

# 7) Create a table showing the number of funds per geography with the following criteria: funds containing both a) a hyphen "-" and b) a lower case letter in the final position of their name.

is.lc = function(x){
  w = nchar(x)
  val = tolower(substr(x,w,w)) == substr(x,w,w)
  val
  }

geo.tab = test_data |>
  filter(grepl("-", Fund.Name),
         is.lc(Fund.Name)) |>
  group_by(Geography) |>
  summarize("Count of Funds" = n()) 

# 8) Add a column to the dataframe called "NewName" to the dataframe with any consecutive sequence of three numbers in the fund name replaced with an asterisk "*" character. Show how you would save the output as an excel file (.xlsx). 

##Im not sure if there is a way to do the saving in base R but you could just copy and paste the data frame
##Or use a package like below
library(writexl)

saved.df = test_data |>
  mutate("NewName" = gsub("\\d{3}", "*", Fund.Name))

write_xlsx(saved.df, path = "C:\\Users\\Charlie/Desktop/test.xlsx")

# 9) Suppose today is 1/1/2022. For active investments, calculate the average investment age (duration) in number of years, broken out by investment Type. 

d = as_date("2022-01-01")

Dur.df = test_data |>
  filter(Active) |>
  mutate(Dur = as.numeric(difftime(d, as.Date(Investment.Date, format = "%m/%d/%Y")) / 365.25)) |>
  group_by(Investment.Type) |>
  summarize("Average Duration" = mean(Dur))
  
# BONUS - 10) Write a function that returns either a) median -> if all input values are unique, or b) mode -> if input values are not all unique.
# Using this function, calculate the output for Type A investments, broken out by strategy. 

MM.fun = function(x){
  
  M.fun = function(x){
    m.tbl = table(x)
    out = as.numeric(names(m.tbl[m.tbl == max(m.tbl)]))
    out
  }
  
  out = if(length(unique(x)) == length(x)){
    median(x)} else {
      M.fun(x)
    }
  out
}

MM.df = test_data |>
  filter(Investment.Type == "Type A") |>
  group_by(Strategy) |>
  summarize("Median or Mode" = MM.fun(Invested))

##As an ending note, I want to apologize if I misinterpreted any of the questions and returned
##an output that does not match what was expected.
##I am happy to show some other personal projects that display more skills if necessary
