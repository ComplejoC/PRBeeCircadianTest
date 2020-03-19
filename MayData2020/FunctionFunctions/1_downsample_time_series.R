#This function will resample time series data (upsample or downsample) to the nearest minute, half-hour, hour, or day. 
#It will also allow for inputation when upsampling and takin the mean, median, or sum when downsampling.
#data must be in tidy format for it to work
downsample_time_series <- function(data = NULL,
                                 datetime_column = "datetime",
                                 amount = 30, 
                                 units = c("minute", "hour", "day", "week"),
                                 method = c("mean", "sum", "median"))
{

#Make sure we have the required packages to run the function
  require(lubridate)
  require(tidyverse)
  require(rlang)

#Managing arguments
  #to work within the tidyverse symtems these chracters must be turned into symbols
datetime_column <- rlang::sym(datetime_column)


  #choose a time point to roll back the time by a certain amount
units <- base::match.arg(units, choices = c("minute", "hour", "day", "week"))

  #handle the anonymous function naming
method <- base::match.arg(method, choices = c("mean", "sum", "median"))
method <- rlang::as_function(method)
method <- base::match.fun(method, c(mean, sum, median))


#flooring dates
floored_dates <- lubridate::floor_date(data %>% dplyr::pull(!!datetime_column), 
                                       unit = lubridate::period(amount, units = units))
data[base::as.character(datetime_column)] <- floored_dates

#pivot data longer for downsampling

data <- data %>% 
  pivot_longer(-!!datetime_column, names_to = "id", values_to = "values")


#downsampling
data <- data %>% 
  dplyr::group_by(!!datetime_column, id) %>%
  dplyr::summarise(downsampled_data =  method(values)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(!!datetime_column, names_from = "id", values_from = "downsampled_data")


  return(data)
}

