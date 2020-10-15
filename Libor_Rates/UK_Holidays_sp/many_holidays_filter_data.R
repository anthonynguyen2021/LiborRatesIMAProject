## Filter data for multiple holidays. Output is called filtered_LIBOR. Slow. 

library(timeDate)
library(tidyverse)
library(rlist)
library(dplyr)
library(lubridate)

data_wanted <- list("GBMayDay", "GBBankHoliday", "GBSummerBankHoliday")
rates <- list("ON", "X1W", "X1M", "X2M", "X3M", "X6M", "X12M")

### output is a list 

days_before_input <- 15
days_after_input <- 15
data_input <- LIBOR
start_date_input <- 2003
end_date_input <- 2019


## Part 1: Filtering for days before, days after 
##########################################################
# used inside of function filter_relative dates 
##########################################################
close_to_holiday <- function(input_date, holiday_list){
  distance_from_all_holidays <- mutate(holiday_list, relative_date = floor(as.period(interval(Date, input_date) ) %/% days(1) ) )%>%
    mutate(absolute_relative_date = abs(relative_date)) %>% arrange(absolute_relative_date)
  return(c(distance_from_all_holidays[1, "relative_date"], year(distance_from_all_holidays[1, "Date"])))
}

# Create Heaviside function for data wrangling
Heaviside <- function(x,a = 0){if(x==0){return(0)} else{return((sign(x)+1)/2)}}

# Function performs regression on a given year in "holiday_data"
linear_regression <- function(year,holiday_data,case){
  
  holiday_data <- holiday_data %>% filter(as.double(holiday_year) == as.double(year))
  formula <- as.formula(paste(as.name(case), "as.integer(relative_date)", sep = "~" ))
  return(lm(formula, data = holiday_data))}
# change relative_date to week.day

# Function filter data in "holiday_data" in a given year
filtered_data <- function(year,holiday_data){
  holiday_data <- holiday_data %>% filter(as.double(holiday_year) == as.double(year))
  return(holiday_data)}




## Add before and after holiday, function 
before_after <- function(dataframe){
  dataframe <- mutate(dataframe, pre = ifelse(relative_date <= 0, TRUE, FALSE))
  return(dataframe)
}

##########################################################


filter_relative_dates <- function(dataframe, holiday, days_before, days_after, year_start, year_end){
  ##############################################################################
  # Create a new table with column name "time stamp" containing entries with format Year-Month-Day 
  ##############################################################################
  days_before_holiday <- days_before
  days_after_holiday <- days_after
  rates_to_track <- dataframe
  
  # Convert date format to year-month-day
  rates_to_track <- rates_to_track  %>% mutate(timestamp = as.Date(dmy(rates_to_track$Date)))  
  ##############################################################################
  
  ##############################################################################
  
  # listHolidays() will give you a list of possible holidays to use
  # In this case, we're using ChristmasDay
  
  holiday_var = holiday
  holidays <- c(holiday_var)  
  Year_begin <- year_start
  Year_end <- year_end
  
  #https://stackoverflow.com/questions/41825688/loop-over-holidays-in-timedate-package
  
  # Create a Table named "holiday_dates" line 59-63
  
  # Holiday        | Date 
  # ChristmasDay     2003-12-25
  # ChristmasDay     2004-12-25
  # ............     ..........
  # ChristmasDay     2009-12-25 
  
  holiday_dates <- do.call(rbind,
                           lapply(holidays, function(i){
                             foo <- match.fun(i)  
                             data.frame(Holiday = i,
                                        Date = as.Date(foo(Year_begin:Year_end)))
                           }))
  #############################################################################
  

  
  rates_to_track_with_holiday_data <- rates_to_track %>% mutate(relative_date = sapply(timestamp, function(x) close_to_holiday(x, holiday_dates)[1]) ) %>% mutate(holiday_year = sapply(timestamp, function(x) close_to_holiday(x, holiday_dates)[2])) %>%
    dplyr::filter(relative_date >= days_before_holiday * -1 & relative_date <= days_after_holiday)
  

  # pre already included 
  #rates_to_track_with_holiday_data <- before_after(rates_to_track_with_holiday_data)
  
  # Creates vector Years_array = ("2004", "2005",...,"2018")
  Years_array <- paste(c(min(as.double(rates_to_track_with_holiday_data$holiday_year)):max(as.double(rates_to_track_with_holiday_data$holiday_year))))
  Number_of_Years <- length(Years_array)
  
  each_year <- list() 
  for (j in c(1:Number_of_Years)){
    
    
    vector_data_table = c()
    
    vector_data_table[[j]] <- filtered_data(Years_array[j],rates_to_track_with_holiday_data)
    
    
    
    for(single_rate in rates){
    # Vector of intercepts beta_0's
      vector_intercept = c()
    
      vector_intercept[j] = coef(linear_regression(Years_array[j],rates_to_track_with_holiday_data,single_rate))[1]
    
    
    
    
    
    
    
    # Add columns which includes recentered rates and Pre column
    
      name <- paste("recentered_rate", single_rate, sep = "_")
      new_col =list(vector_data_table[[j]][, single_rate] - vector_intercept[j])
      names(new_col) <- name
      vector_data_table[[j]] <- cbind(vector_data_table[[j]], new_col)  
    
    }
    
    each_year <- list.append(each_year, vector_data_table[[j]])
    
    
    
  }
  
  
  # Concatenate my tabular data from each year
  largeData <- data.frame()
  for(j in c(1:Number_of_Years)){
    largeData <- rbind(largeData, each_year[[j]])
  }
  
  
  largeData <- largeData %>% mutate(., Pre = lapply(relative_date, Heaviside))
  
  
  
  return(largeData)
}






### Start making the output list here 

filtered_LIBOR <- list()
for (item in data_wanted){
  filtered_LIBOR <- list.append(filtered_LIBOR, filter_relative_dates(data_input, item, days_before_input, days_after_input, start_date_input, end_date_input))
  
  
}


names(filtered_LIBOR) <- data_wanted


view(filtered_LIBOR)


