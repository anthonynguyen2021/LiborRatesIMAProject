library(rlist)

## This file requires an output list from many_holidays_filter_data.R. It calculates statistics for each Holiday and for each LIBOR rate. It creates a matrix called value_table. 



holidays <- filtered_LIBOR  ## This should be your result from many_holidays_filter_data.R
rates <- list("ON", "X1W", "X1M", "X2M", "X3M", "X6M", "X12M")
centered_rates <- list("recentered_rate_ON", "recentered_rate_X1W", "recentered_rate_X1M", "recentered_rate_X2M", "recentered_rate_X3M", "recentered_rate_X6M", "recentered_rate_X12M")

value_table <- c(names(holidays)) # initialize the output table with a column of holidays 




for (single_rate in centered_rates){

a_rate_value <- c()

for (holiday in holidays){
  formula <- as.formula(paste(single_rate, "as.integer(Pre)", sep = "~"))   ## this and the next line is what you change to use a different model 
  model <- lm(formula, data = holiday)       ### 
  estimate <- summary(model)$coef["as.integer(Pre)", "Estimate"]   ## change this line and next two lines to add different data to the table 
  p_value <- summary(model)$coef["as.integer(Pre)", "Pr(>|t|)"]
  r_squared <-summary(model)$adj.r.squared
  a_rate_value <- rbind(a_rate_value, list(estimate, p_value, r_squared, summary(model)))
}

value_table <- cbind(value_table, a_rate_value)

}



## make the column names. You will need to change this if you add different data to the table

column_names <- list()

for (rate in centered_rates){
  new_sublist <- list(paste("Pre", rate, sep = " "), paste("p value", rate, sep = " "), paste(rate, "Adj Rsq", sep = " "), paste(rate, "Summary", sep = " ") )
  column_names <- c(column_names, new_sublist)
}

column_names <- list.append("HOLIDAY", column_names)

colnames(value_table) <- column_names 

######################################################################################################