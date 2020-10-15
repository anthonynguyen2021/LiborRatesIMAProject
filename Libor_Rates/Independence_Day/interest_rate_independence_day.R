# We are examining data from 06-27 to 07-11 for each year.
# I.e. 7 days before 7-4 and 7 days after

# Import Data
### WARNING: Insert your path in read.csv
LIBOR <- read.csv("/Users/anthonynguyen/Desktop/IMA_bootcamp/Week_4/LIBOR_RATES/July_16/code_proto_7pm/Libor_Rates/LIBOR USD.csv")

# Use Libraries
library(lubridate)
library(dplyr)
library(ggplot2)

# Add column "Percentage Change"
LIBOR_PC = cbind(data.frame(Percent_Change_ON = 100 * diff(log(LIBOR$ON)), Percent_Change_1W = 100 * diff(log(LIBOR$X1W))), 
                 LIBOR[-1, ])

# Data Filtering
July = LIBOR_PC %>%
  ##convert the Date into a timestamp class
  mutate(timestamp = dmy(LIBOR_PC$Date)) %>%
  mutate(Year = year(timestamp), MonthDay = format(timestamp, "%m%d")) %>%
  filter(MonthDay >= "0627" & MonthDay <= "0711") %>%
  mutate(Pre = ifelse(MonthDay > "0704", "Post", ifelse(MonthDay <= "0704", "Pre", "Post"))) 

# July has some NA entries, so we need to remove rows with NA entries
July <- na.omit(July)

# Write csv file named "July"
write.csv(July, "July.csv")

# Filter our Data to examine the year 2018-2019
One_Year = July %>%
  filter(between(timestamp, as.Date("2018-06-27"), as.Date("2019-07-11")))

# Plot MonthDay versus ON for every year
ggplot(July, aes(y = ON, x = MonthDay, group = Year, color = Year)) + geom_point() + geom_line()

# Plot Data for year 2018-2019
ggplot(One_Year, aes(y = ON, x = timestamp)) + geom_point() 


# Summary of Regression model
summary(lm(Percent_Change_ON ~  Pre , data = July))
summary(lm(Percent_Change_1W ~  Pre , data = July))

# Best Fit Line plot with data points for combined Pre + Post
ggplot(July, aes(y = Percent_Change_1W, x = timestamp)) + geom_point() + geom_smooth(method = "lm")

# Best Fit Line plot with data points for Pre and Post separately
ggplot(July, aes(y = Percent_Change_1W, x = timestamp, group = Pre, color = Pre)) + geom_point() + geom_smooth(method = "lm")


