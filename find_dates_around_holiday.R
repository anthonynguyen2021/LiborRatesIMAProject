LIBOR <- read.csv("LIBOR_USD.csv")

library(lubridate)
library(dplyr)
library(timeDate)

##########################################################
# returned table is called rates_to_track_with_holiday_data
###########################################################
days_before_holiday <- 7
days_after_holiday <- 7
holidays <- c("ChristmasDay") # listHolidays() will give you a list of possible holidays to use 
rates_to_track <- LIBOR

rates_to_track <- rates_to_track  %>% mutate(timestamp = as.Date(dmy(rates_to_track$Date)))  # change your date format to year-month-day
##########################################################



##############################################################################
###This is the list of holiday dates, I stole it from  https://stackoverflow.com/questions/41825688/loop-over-holidays-in-timedate-package 
holiday_dates = do.call(rbind,
        lapply(holidays, function(i){
          foo <- match.fun(i)  
          data.frame(Holiday = i,
                     Date = as.Date(foo(2003:2019)))
          
        }))
#########################################################################


## a function that finds how far a date is from the given holiday (the minimum distance). returns a column of this distance and the year of the holiday

close_to_holiday <- function(input_date){
  distance_from_all_holidays <- mutate(holiday_dates, relative_date = as.period(interval(Date, input_date) ) %/% days(1) ) %>%
    mutate(absolute_relative_date = abs(relative_date)) %>% arrange(absolute_relative_date)
  return(c(distance_from_all_holidays[1, "relative_date"], year(input_date)))
}

# next two functions are a hack to make the dataframe wrangling work, can be made better 
get_relative_date <- function(input_date){
  return(close_to_holiday(input_date)[1])
}

get_relative_holiday_year <- function(input_date){
  return(close_to_holiday(input_date)[2])
}



####### Finally, add relative holiday data to the dataframe and filter for the days you need 

rates_to_track_with_holiday_data <- rates_to_track %>% mutate(relative_date = lapply(timestamp, get_relative_date) ) %>% mutate(holiday_year = lapply(timestamp, get_relative_holiday_year)) %>%
  filter(relative_date >= days_before_holiday * -1 & relative_date <= days_after_holiday)



