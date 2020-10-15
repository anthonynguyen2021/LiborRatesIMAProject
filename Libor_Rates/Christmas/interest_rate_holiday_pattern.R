##import libor data 
LIBOR <- read.csv("C:/Users/Ray/Desktop/project 1/LIBOR USD.csv")

library(lubridate)
library(dplyr)
library(ggplot2)

##add a column of percentage change 
LIBOR_PC = cbind(data.frame(Percent_Change_ON = 100 * diff(log(LIBOR$ON)), Percent_Change_1W = 100 * diff(log(LIBOR$X1W))), 
                 LIBOR[-1, ])

Dec_Jan = LIBOR_PC %>%
  ##convert the Date into a timestamp class
  mutate(timestamp = dmy(LIBOR_PC$Date)) %>%
  mutate(Year = year(timestamp), MonthDay = format(timestamp, "%m%d")) %>%
  filter(MonthDay >= "1217" | MonthDay <= "0108") %>%
  mutate(Pre = ifelse(MonthDay > "1224", "Post", ifelse(MonthDay <= "0131", "Post", "Pre"))) 
  

  
write.csv(Dec_Jan, "Dec_Jan.csv")

One_Year = Dec_Jan %>%
  filter(between(timestamp, as.Date("2018-12-17"), as.Date("2019-01-08")))

ggplot(Dec_Jan, aes(y = ON, x = MonthDay, group = Year, color = Year)) + geom_point() + geom_line()

ggplot(One_Year, aes(y = ON, x = timestamp)) + geom_point() 

###percentage change
############problem 
##add a column of percentage change of overnight interest rate percentage change
#mutate(Percenate_Change = diff(log(ON)))

summary(lm(Percent_Change_ON ~  Pre , data = Dec_Jan))

summary(lm(Percent_Change_1W ~  Pre , data = Dec_Jan))

ggplot(Dec_Jan, aes(y = Percent_Change_1W, x = timestamp)) + geom_point() + geom_smooth(method = "lm")
ggplot(Dec_Jan, aes(y = Percent_Change_1W, x = timestamp, group = Pre, color = Pre)) + geom_point() + geom_smooth(method = "lm")
