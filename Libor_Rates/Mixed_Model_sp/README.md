## Mixed Model Statistics

### many_holidays_filter_data.R
Inputs: LIBOR table, filtering information: days before, days after, holidays, start year, end year
Ouput: A list where each item is a dataframe for each Holiday with the requested filters

### finish.R
Input: the list from many_holidays_filter_data.R
Output: A matrix with statistics data, ex:


|-------------------|------------|------------|------------|
| HOLIDAY           | ON PreTRUE | ON t value | ON Summary |
|-------------------|------------|------------|------------|
| ChristmasDay      | number     | number     | matrix     |
| USIndependenceDay | number     | number     | matrix     |  
