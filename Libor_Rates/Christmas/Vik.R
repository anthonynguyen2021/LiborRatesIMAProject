library(lubridate)
library(dplyr)
library(ggplot2)
library(magrittr)

Years_array <- c("2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")
# WARNING CHANGE PATH BEFORE USAGE!!!
LIBOR <- read.csv("/Users/anthonynguyen/Desktop/IMA_bootcamp/Week_4/LIBOR_RATES/July_16/code_proto_7pm/Vik_model/LIBOR USD.csv")

LIBOR <- na.omit(LIBOR)

# For each year from 2004 to 2019
# Perform a linear regression rate = days of year (categorical variable)

# Initialize vector of regression models
Dec_Jan = LIBOR %>%
  ##convert the Date into a timestamp class
  mutate(timestamp = dmy(LIBOR$Date)) %>%
  mutate(Year = year(timestamp), MonthDay = format(timestamp, "%m%d")) %>%
  filter(MonthDay >= "1217" | MonthDay <= "0108") %>% mutate(Pre = ifelse(MonthDay > "1224", "Post", ifelse(MonthDay <= "0131", "Post", "Pre")))

# year in {"2004",....,"2018"} where "20XX" refers to "20XX-20X(X+1)"
# USAGE: model <- lin_regression("2017",Dec_Jan)
lin_regression <- function(year,Data_Filter){
  Winter_1 <- Data_Filter %>% filter(Year >= as.double(year)) %>% filter(Year <= as.double(year)+1) %>% filter(Year == (as.double(year)+1) & MonthDay <= "0108")
  Winter_2 <- Data_Filter %>% filter(Year >= as.double(year)) %>% filter(Year <= as.double(year)+1) %>% filter(Year == as.double(year) & MonthDay >= "1218")
  Winter <- rbind(Winter_1,Winter_2)
  return(lm(ON ~ Week.day,data=Winter))}

# Function returns the filtered data table
# USAGE: table_1 <- filtered_data("2018",Dec_Jan)
filtered_data <- function(year,Data_Filter){
  Winter_1 <- Data_Filter %>% filter(Year >= as.double(year)) %>% filter(Year <= as.double(year)+1) %>% filter(Year == (as.double(year)+1) & MonthDay <= "0108")
  Winter_2 <- Data_Filter %>% filter(Year >= as.double(year)) %>% filter(Year <= as.double(year)+1) %>% filter(Year == as.double(year) & MonthDay >= "1218")
  Winter <- rbind(Winter_1,Winter_2)
  return(Winter)}

# Vector of intercepts beta_0s
vector_intercept = c()
j = 0
for(i in Years_array){
  j = j+1
  model <- lin_regression(i,Dec_Jan)
  vector_intercept[j] = coef(model)[1]
}

# vector of data from year 2004-2019
vector_data_table = c()
j = 0
for(i in Years_array){
  j = j+1
  Winter <- filtered_data(i,Dec_Jan)
  vector_data_table[[j]] <- Winter
}

# New table shift by intercept
# Access table by vector_data_table[[j]] j = 1 refers to "2004" to j = 15 to "2018"s
for(j in c(1:15)){
  vector_data_table[[j]] <- vector_data_table[[j]] %>% mutate(shifted_data_int = ON - vector_intercept[j])
}

# year in {"2004",....,"2018"} where "20XX" refers to "20XX-20X(X+1)"
# USAGE: model <- lin_regression("2017",Dec_Jan)
lin_regression_two_predictors <- function(year,Data_Filter){
  Winter_1 <- Data_Filter %>% filter(Year >= as.double(year)) %>% filter(Year <= as.double(year)+1) %>% filter(Year == (as.double(year)+1) & MonthDay <= "0108")
  Winter_2 <- Data_Filter %>% filter(Year >= as.double(year)) %>% filter(Year <= as.double(year)+1) %>% filter(Year == as.double(year) & MonthDay >= "1218")
  Winter <- rbind(Winter_1,Winter_2)
  return(lm(ON ~ Week.day + Pre,data=Winter))}



