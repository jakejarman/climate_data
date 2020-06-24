## Utilising the data that Hemda has provided ##

library(tidyverse)
library(lubridate)

## Reading in the file ####

weather <- read_csv("kavanagh_weather.csv")
weather <- select(weather, day, max_temp, min_temp, solar_radn, wind_run, rain, evapo_trans)
weather$day <- dmy(weather$day)

# https://stackoverflow.com/questions/33221425/how-do-i-group-my-date-variable-into-month-year-in-r

weather <- weather %>% mutate(month = format(day, "%m"),
                   year = format(day, "%Y"))

weather$month <- as.numeric(weather$month)
# weather$month <- month.abb[weather$month]

weather$year <- as.numeric(weather$year) 


# weather %>% group_by(month = floor_date(day, "month")) %>%
#   summarise(rain = sum(rain))

# https://www.earthdatascience.org/courses/earth-analytics/time-series-data/summarize-time-series-by-month-in-r/
month_weather <- weather %>%
  group_by(month) %>%
  summarise(mean_max_temp = mean(max_temp),
            mean_min_temp = mean(min_temp),
            mean_solar_radn = mean(solar_radn),
            mean_wind_run = mean(wind_run))

# Looking at the average rainfall from 1980 to 2016
monthly_rainfall <- weather %>%
  filter(day< "2016-01-01") %>%
  group_by(month, year) %>%
  summarise(rainfall = sum(rain))

monthly_rainfall2 <- monthly_rainfall %>%
  group_by(month) %>%
  summarise(ave_rainfall = mean(rainfall))

# Looking at the rainfall over the 2017 2018 season
rainfall_2018_2019 <- weather %>%
  filter(between(day, as.Date("2017-06-01"), as.Date("2019-05-31"))) %>%
  group_by(month, year) %>%
  summarise(rainfall = sum(rain))


monthly_rainfall2 <- monthly_rainfall %>%
  group_by(month) %>%
  summarise(ave_rainfall = mean(rainfall))


## Reading in the cliflo data from Kavanagh ####

cliflo_weather <- read_csv("cliflo_kavanagh.csv")
cliflo_weather$date <- dmy(cliflo_weather$date)
cliflo_weather <- cliflo_weather %>%
  mutate(m = format(date, "%m"),
         y = format(date, "%Y"))

kavanagh_trial_weather <- cliflo_weather %>%
  group_by(m, y) %>%
  summarise(rainfall = sum(rain))

k_rainfall <- kavanagh_trial_weather %>%
  group_by(m) %>%
  summarise(ave_rainfall = mean(rainfall))

k_rainfall_2017_2018 <- kavanagh_trial_weather %>%
  filter(between(m, as.Date("2017-06-01"), as.Date("2019-04-31"))) %>%
  group_by(m) %>%
  summarise(rainfall = mean(rainfall))
