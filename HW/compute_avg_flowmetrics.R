#' average flowmetrics
#'
#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year
#' @param avg_flow_months The start of the water year October
#' @return annual_avg_err, annual_avg_corr, avg_month_cor, avg_month_err

compute_avg_flowmetrics = function(m,o, month, day, year,wy, avg_flow_months=10, avg_flow_day = 1) {
  
  flow = cbind.data.frame(m,o, month, day, year,wy)
  # first lets get average yearly values
  
  tmp = flow %>% group_by(wy) %>% summarize(avg_o=mean(o), avg_m=mean(m))
  
  annual_avg_cor = cor(tmp$avg_m, tmp$avg_o)
  
  # now lets get daily values
  tmp = flow %>% group_by(month, year) %>% summarize(model=sum(m), obs=sum(o))
  # now extract october
  avg = subset(tmp, month %in% avg_flow_months)
  avg_month_cor=cor(avg$model, avg$obs)
  
  # daily values
  tmp = flow %>% group_by(day,month ) %>% summarize(model = sum(m), obs = sum(o))
  # extract october
  avg_day = subset(tmp,day %in% avg_flow_day)
  avg_day_cor = cor(avg_day$model, avg_day$obs)
  
  
  # return(list(annual_avg_cor=annual_avg_cor,
  #             avg_month_cor=avg_month_cor,
  #             avg_day_cor = avg_day_cor))
  
  
 paste("Average yearly, monthly, daily correlation",round(mean(annual_avg_cor, avg_month_cor, avg_day_cor),4))
  
}