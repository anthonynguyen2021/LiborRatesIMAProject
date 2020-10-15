library(rlist)

## This file requires an output list from many_holidays_filter_data.R. It calculates statistics for each Holiday and for each LIBOR rate. It creates a matrix called value_table. 



holidays <- filtered_LIBOR  ## This should be your result from many_holidays_filter_data.R
rates <- list("ON", "X1W", "X1M", "X2M", "X3M", "X6M", "X12M")

value_table <- c(names(holidays)) # initialize the output table with a column of holidays 




for (single_rate in rates){

a_rate_value <- c()

for (holiday in holidays){
  formula <- as.formula(paste(single_rate, "pre + (1|holiday_year)", sep = "~"))   ## this and the next line is what you change to use a different model 
  model <- lmer(formula, data = holiday, REML = FALSE)       ### 
  estimate <- summary(model)$coef["preTRUE", "Estimate"]   ## change this line and next two lines to add different data to the table 
  t_value <- summary(model)$coef["preTRUE", "t value"]
  a_rate_value <- rbind(a_rate_value, list(estimate, t_value, summary(model)))
}

value_table <- cbind(value_table, a_rate_value)

}



## make the column names. You will need to change this if you add different data to the table

column_names <- list()

for (rate in rates){
  new_sublist <- list(paste(rate, "preTrue", sep = " "), paste(rate, "t value", sep = " "), paste(rate, "Summary", sep = " "))
  column_names <- c(column_names, new_sublist)
}

column_names <- list.append("HOLIDAY", column_names)

colnames(value_table) <- column_names 

######################################################################################################