# This R script will filter the data of (a) US Treasury Yield Curve Rates, (b) US Treasury Bill Rates (c) US TREASURY High Quality Market Corporate Bond Yield Curve Spot Rates between December 17 2004 - January 08 2019
# Data is pulled from https://www.quandl.com/data/USTREASURY/HQMYC-High-Quality-Market-Corporate-Bond-Yield-Curve-Spot-Rates
# IMPORTANT: Change the path in line 8,10,12 so this script works on your machine. For mac/linux users, use pwd to print current working directly and CMD (forget what command to print current directory)

#---------------------------------------------------------------------------------------------------------------------------------------

#import TREASURY data 
TREASURY_YIELD <- read.csv("/Users/anthonynguyen/Desktop/IMA_bootcamp/Week_4/TREASURY_RATES/USTREASURY_YIELD.csv")
#import Bill Rates data
TREASURY_BILL <- read.csv("/Users/anthonynguyen/Desktop/IMA_bootcamp/Week_4/TREASURY_RATES/USTREASURY_BILL_RATES.csv")
#import High Quality Market Corporate Bond Yield Curve Spot Rates (HQMCBYCSR)
TREASURY_HQMCBYCSR <- read.csv("/Users/anthonynguyen/Desktop/IMA_bootcamp/Week_4/TREASURY_RATES/USTREASURY-HQMYC.csv")

#----------------------------------------------------------------------------------------------------------------------------------------

# use libraries
library(lubridate)
library(dplyr)
library(ggplot2)

#---------------------------------------------------------------------------------------------------------------------------------------
# Filter Treasury Yield Curve Rates Data for the columns with Month 1 and Month 2
TREASURY_YIELD <- TREASURY_YIELD %>% select(Date,X1.MO,X2.MO) 
TREASURY_YIELD$Date <- as.Date(TREASURY_YIELD$Date)
TREASURY_YIELD <- TREASURY_YIELD %>% filter(between(Date,as.Date("2004-12-17"), as.Date("2019-01-08")))

# Filter Treasury Bill Rates Data
TREASURY_BILL <- TREASURY_BILL %>% select(Date,X4.Wk.Bank.Discount.Rate,X4.Wk.Coupon.Equiv,X8.Wk.Bank.Discount.Rate,X8.Wk.Coupon.Equiv)
TREASURY_BILL$Date <- as.Date(TREASURY_BILL$Date)
TREASURY_BILL <- TREASURY_BILL %>% filter(between(Date,as.Date("2004-12-17"), as.Date("2019-01-08")))

# Filter Treasury HQMCBYCSR Data
TREASURY_HQMCBYCSR <- TREASURY_HQMCBYCSR %>% select(Month,X1.0,X2.0)
TREASURY_HQMCBYCSR$Month <- as.Date(TREASURY_HQMCBYCSR$Month)
TREASURY_HQMCBYCSR <- TREASURY_HQMCBYCSR %>% filter(between(Month,as.Date("2004-12-17"), as.Date("2019-01-08")))

#---------------------------------------------------------------------------------------------------------------------------------------
# Column Renaming

# Rename for Treasury Yield Curve Rates
colnames(TREASURY_YIELD)[2] <- "1 Month"
colnames(TREASURY_YIELD)[3] <- "2 Month"

# Rename Columns in Treasury Bill Data
colnames(TREASURY_BILL)[2] <- "4 Weeks Bank Discount Rate"
colnames(TREASURY_BILL)[3] <- "4 Weeks Coupon Equivalent"
colnames(TREASURY_BILL)[4] <- "8 Weeks Bank Discount Rate"
colnames(TREASURY_BILL)[5] <- "8 Weeks Coupon Equivalent"

# Rename Columns in HQMCBYCSR Data
colnames(TREASURY_HQMCBYCSR)[1] <- "Date"
colnames(TREASURY_HQMCBYCSR)[2] <- "4 Weeks"
colnames(TREASURY_HQMCBYCSR)[3] <- "8 Weeks"

#---------------------------------------------------------------------------------------------------------------------------------------

# Filter NA Data in TREASURY Data

# 0 to leave NA Data
# 1 to filter NA Data
case_switch_TREASURY_BILL = 0

if(case_switch_TREASURY_BILL == 1){
  TREASURY_BILL <- na.omit(TREASURY_BILL)
  TREASURY_YIELD <- na.omit(TREASURY_YIELD)
  TREASURY_HQMCBYCSR <- na.omit(TREASURY_HQMCBYCSR)
  }


#---------------------------------------------------------------------------------------------------------------------------------------



