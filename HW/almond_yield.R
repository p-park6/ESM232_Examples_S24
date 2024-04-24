#' Finding the almond yield from climate observations
#'
#' @param dataset The dataset that you are interested in that includes min and max for temperature, as well as precipitation data
#' @param month_num The number of month you would like to see the yield for (enter only a number from 1 to 12)
#'
#' @return The minimum temperature (ton/acre), the maximum temperature (ton/acre), and the mean temperature of that month (ton/acre)
#' @export
#'
#' @examples
almond_yield = function(dataset, month_num){
  #clean up dataset to have information we are interested in (ex. max and min of temp and sum of precip)
  climate_data_month <- dataset %>%
    select(-c(wyd, wy)) %>% #get rid of columns we are not interested in
    group_by(month, year) %>% #get monthly readings
    summarize(max_temp = mean(tmax_c), #get mean max temp of that month
              min_temp = mean(tmin_c), #get mean min temp of that month
              sum_precip = sum(precip)) #get sum of precip of that month
  
  #subset to only get precip data of the month before
  precip <- subset(climate_data_month, 
                   month == ifelse(month_num == 1, 12, month_num-1))$sum_precip
  
  #subset to only get min_temp data of the month we are interested in
  min_temp <- subset(climate_data_month, month == month_num)$min_temp
  
  #create an empty list for the monthly_yield to append to
  yield_list <- list()
  
  #create a for loop to go through the whole list and append the results to the empty list created previously
  for (i in seq(1:length(precip))){
    monthly_yield = -0.015*min_temp[i] - 0.0046*(min_temp[i]^2) - 0.07*precip[i] + 0.0043*(precip[i]^2)+0.28
    yield_list <- append(yield_list, monthly_yield) #append onto the list
  }
  
  #print out results (max, min, and mean temp)
  print(paste0("Max: ", round(max(unlist(yield_list)), 5), " ton/acre"))
  print(paste0("Min: ", round(min(unlist(yield_list)), 5), " ton/acre"))
  print(paste0("Mean: ", round(mean(unlist(yield_list)), 5), " ton/acre"))
  
}

almond_yield(climate_data, 2)
