library(tidyverse)
library(xml2)
library(rvest)
library(rlist)
library(ggplot2)

#Introduction ---------------------------------------
#Problem: Write a function for obtaining an average FX rates from the Czech National Bank site
#The function should return a matrix of years and average FX rates for the given year for the Honduran Lempira



years <- c(2004:2021)
urls <- c()

for (year in years) {
   urls <- c(urls, sprintf("https://www.cnb.cz/en/financial-markets/foreign-exchange-market/fx-rates-of-other-currencies/fx-rates-of-other-currencies/year.txt?year=%s&format=txt", year))
}


ldf <- lapply(urls, function(i) {
   read.csv(i, header = T, sep = "|")
})


df_m <- lapply(ldf, function(i) {
   select(i, "Date", "X1.HNL")
})

df_m <- do.call(rbind, df_m)

df_m$Date <- format(as.Date(df_m$Date, format="%d.%m.%Y"),"%Y")

df_f <- df_m %>% group_by(Date) %>% summarise(avg_fx_rates = mean(X1.HNL))










