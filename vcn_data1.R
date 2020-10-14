## Utilising the data that Hemda has provided ##

library(tidyverse)
library(lubridate)
library(ggpubr)
library(fuzzyjoin)
library(ggpmisc)
library(scales)
library(flextable)
library(egg)
library(extrafont)
#loadfonts(device = "win")
windowsFonts(Times = windowsFont("TT Times New Roman"))
library(aTSA)
library(cvequality)
library(clifro)

#-------------------------------# Reading in the file ####

weather <- read_csv("kavanagh_weather.csv")
weather <- select(weather, day, max_temp, min_temp, solar_radn, wind_run, rain, evapo_trans)
weather$day <- dmy(weather$day)

# https://stackoverflow.com/questions/33221425/how-do-i-group-my-date-variable-into-month-year-in-r

weather <- weather %>% mutate(month = format(day, "%m"),
                              year = format(day, "%Y"))

weather$month <- as.numeric(weather$month)
# weather$month <- month.abb[weather$month]

weather$year <- as.numeric(weather$year) 


#### For Linda ####
# write_csv(weather,
#           "Kavanagh farm virtual climate station network 1980-2020.csv")

# weather %>% group_by(month = floor_date(day, "month")) %>%
#   summarise(rain = sum(rain))

# https://www.earthdatascience.org/courses/earth-analytics/time-series-data/summarize-time-series-by-month-in-r/
month_weather <- weather %>%
  group_by(month) %>%
  summarise(mean_max_temp = mean(max_temp),
            mean_min_temp = mean(min_temp),
            mean_solar_radn = mean(solar_radn),
            mean_wind_run = mean(wind_run),
            mean_evapo = mean(evapo_trans))

# Looking at the average rainfall from 1980 to 2017-05-31
monthly_rainfall <- weather %>%
  filter(day< "2017-05-31") %>%
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


#---------------------------# Reading in the cliflo data from Kavanagh ####

cliflo_weather <- read_csv("cliflo_kavanagh.csv")
cliflo_weather <- select(cliflo_weather, date, rain)
# Changing the character date column to Date class
cliflo_weather$date <- dmy(cliflo_weather$date)


# Making two new columns that are the month and year as integar values
cliflo_weather <- cliflo_weather %>%
  mutate(m = format(date, "%m"),
         y = format(date, "%Y"))
cliflo_weather$m <- as.numeric(cliflo_weather$m)
cliflo_weather$y <- as.numeric(cliflo_weather$y)

# New dataframe that has the monthly sum of rainfall grouped by the month and the year
cliflo_month_rain <- cliflo_weather %>%
  group_by(m, y) %>%
  summarise(rainfall = sum(rain))

# New dataframe that has the average of each month based upon all the months
cliflo_month_sum <- cliflo_month_rain %>%
  group_by(m) %>%
  summarise(ave_rainfall = mean(rainfall))

# New dataframe that has monthly sums of rainfall for 2017 to 2019
k_rain_2017_2018 <- cliflo_weather %>%
  filter(between(date, as.Date("2017-06-01"), as.Date("2019-05-01"))) %>%
  group_by(m, y) %>%
  summarise(rainfall = sum(rain))


#---------- Monthly summary of rainfall for the period from 2004-01-30 to 2018-12-31, with time plot####


cliflo_12year <- cliflo_weather %>%
  filter(between(date, as.Date("2004-01-31"), as.Date("2017-05-31"))) %>%
  group_by(m, y) %>%
  summarise(rainfall = sum(rain)) %>%
  group_by(m) %>%
  summarise(ave_rainfall = mean(rainfall))

vcn_12year <- weather %>%
  filter(between(day, as.Date("2004-01-31"), as.Date("2017-05-31"))) %>%
  group_by(month, year) %>%
  summarise(rainfall = sum(rain)) %>%
  group_by(month) %>%
  summarise(ave_rainfall = mean(rainfall))


com_12year <- bind_cols(vcn_12year, cliflo_12year)

com_12year <- rename(com_12year,
                     vcn = ave_rainfall,
                     cliflo = ave_rainfall1)

com_12year <- select(com_12year,
                     m,
                     vcn,
                     cliflo)

com_12year1 <- mutate(com_12year,
                     diff = ((vcn-cliflo)/vcn))

com_12year$m <- month(com_12year$m)

ggplot(com_12year) +
       geom_point(aes(vcn, cliflo)) +
  stat_cor(aes(vcn, cliflo)) +
  geom_abline(aes(intercept = 0,
                  slope = 1))
ggsave("Corralation comparison 12 year data.png")  

  
ggplot(com_12year) +
  geom_smooth(aes(m, vcn), se = FALSE) +
  geom_smooth(aes(m, cliflo), se = FALSE)
ggsave("Smooth line 12 year data.png")   


# Have made summaries of the average rainfall of each month for VCN and Cliflo from 2004 to 2017
# and have then plotted both in scatter and line graph.
# Not sure if it would be worth looking at each month individually and comparing them

#----------------------------------------------------------------------------------------------------------
## Generating the actual rainfall data from the trial period from cliflo data ####

cliflo_trial_data <- cliflo_weather %>%
  filter(between(date, as.Date("2017-06-01"), today())) %>%
  group_by(m, y) %>%
  summarise(mon_rainfall = sum(rain))

# I have included the today function, so every time I run it it will generate to the current date, 
# but means I will need to update the master csv file first.

# Next job is to look at the weather (VCN) data to identify trends.
# Could split it into three 12 year blocks? Or 6 six year blocks?
# And I need to make a new column of season

#-------------------------------------------------------------------------------------------------#####
## Working with VCN data to analyse season trends####
# Reading in the csv file from Hemda again and calling it vcn_weather to distingush it
# plus renaming columns and making new day, month and year columns
vcn_weather <- read_csv("kavanagh_weather.csv")
vcn_weather <- select(weather, day, max_temp, min_temp, solar_radn, wind_run, rain, evapo_trans)
vcn_weather <- rename(vcn_weather,
       date = day)

vcn_weather <- vcn_weather %>% mutate(day = format(date, "%d"),
                                      month = format(date, "%m"),
                                      year = format(date, "%Y"))

# Filtering out the -999 rows
vcn_weather <- vcn_weather %>% filter(max_temp >=0,
                       solar_radn >= 0,
                       evapo_trans >= 0)

#-----------------------------------------------------------------------------------#####
# Making a season column that is based upon the month column ####

# Attempt 1 from https://stackoverflow.com/questions/49702002/creating-season-variable-by-month-with-dplyr-in-r
keyval <- data.frame(month = c("12", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11"), 
                     season = rep(c("summer", "autumn", "winter", "spring"), each = 3),
                     stringsAsFactors = FALSE)

# left_join(data, keyval)

# left_join(vcn_weather, keyval, by = "month")
# Did not work ^ to begin with because of incompatible numeric and character class of month column
# Need to change the class of month to character in both

keyval$month <- as.character(keyval$month)

# Now I can inner join them together because there are lots of duplicates
vcn_weather <- inner_join(vcn_weather, keyval, by = "month")

#-----------------------------------------------------------------------------------------------------------
# Creating another column that assigns a row to one of six 6-year periods starting 1980-1985, 1986-1991, 1992-1997, 1998-2003, 2004-2009, 2010-2015

# vcn_weather %>% mutate(period = format(date, (between(date, as.Date("1980-01-01"), as.Date("1985-12-31")))), "1")
# Won't ^ work

# mutate(vcn_weather$date %in% c())

# first_go <- mutate(vcn_weather,
#        my_interval = if_else(vcn_weather$date %within% interval("1980-01-01", "1985-12-31"), "1",
#                              if_else(vcn_weather$date %within% interval("1980-01-01", "1985-12-31"), "2", "xyz")))
# 
# second_go <- mutate(vcn_weather,
#                    my_period = if_else(vcn_weather$date %within% interval("1980-01-01", "1985-12-31"), "1",
#                                if_else(vcn_weather$date %within% interval("1986-01-01", "1991-12-31"), "2",
#                                if_else(vcn_weather$date %within% interval("1992-01-01", "1997-12-31"), "3",
#                                if_else(vcn_weather$date %within% interval("1998-01-01", "2003-12-31"), "4",
#                                if_else(vcn_weather$date %within% interval("2004-01-01", "2009-12-31"), "5",
#                                if_else(vcn_weather$date %within% interval("2010-01-01", "2016-12-31"), "6",
#                                        "")))))))


p1 <- interval("1980-01-01", "1985-12-31")
p2 <- interval("1986-01-01", "1991-12-31")
p3 <- interval("1992-01-01", "1997-12-31")
p4 <- interval("1998-01-01", "2003-12-31")
p5 <- interval("2004-01-01", "2009-12-31")
p6 <- interval("2010-01-01", "2016-12-31")

third_go <- mutate(vcn_weather,
                   my_period = if_else(vcn_weather$date %within% p1, "1",
                               if_else(vcn_weather$date %within% p2, "2",
                               if_else(vcn_weather$date %within% p3, "3",
                               if_else(vcn_weather$date %within% p4, "4",
                               if_else(vcn_weather$date %within% p5, "5",
                               if_else(vcn_weather$date %within% p6, "6",
                                "NA")))))))

#third_go$my_period <- as.factor(third_go$my_period)
third_go$my_period <- as.numeric(third_go$my_period)
third_go <- third_go %>% !is.na(third_go$my_period)

third_go_1 <- third_go %>%
  group_by(my_period, year, month) %>%
  summarise(month_rain = sum(rain))


ggplot(data = third_go_1,
       aes(x = my_period,
           y = month_rain,
           group = my_period)) +
  geom_boxplot() +
  stat_compare_means() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point", size = 3, show.legend = FALSE) +
  stat_summary(fun.y = mean, colour = "red", geom = "text", show.legend = FALSE, vjust = -0.7, aes(label = round(..y.., digits = 1))) +
  facet_wrap(~third_go_1$month)

third_go_1 <- third_go %>%
  group_by(my_period, year, month) %>%
  summarise(month_rain = sum(rain))

#-----------The proper way to assign a new column based upon the date range####----------------------

# Fourth go is after Mark sent his email with the link https://stackoverflow.com/questions/40380112/categorize-continuous-variable-with-dplyr
# But is not working because of the format
fourth_go <- vcn_weather %>% 
  mutate(new_date = as.POSIXct(date, format = "%d/%m%Y"))
class(fourth_go$new_date)

fourth_go <- fourth_go %>%
  mutate(period_sixs = cut(new_date,
                           breaks = seq(as.POSIXct.Date("1980-01-01"), as.POSIXct.Date("1985-12-31"), years(1))))

fourth_go <- vcn_weather %>%
  mutate(period_sixs = cut(date,
                           breaks = seq(ymd("1980-01-01"), ymd("2016-01-01"), '6 years'), 
                            labels = c("p1", "p2", "p3", "p4", "p5", "p6")))

# Checking the functions
# class(ymd("1985-12-31"))
# class(years(1))
# years(1)
# ?seq
# seq(ymd("1980-01-01"), ymd("2015-12-31"), '6 years')
# seq.int(ymd("1980-01-01"),)
# class(interval("1980-01-01"))


#----------Summarising data to present in the materials and methods####-------------
# Provides the rainfall from VCN from the last 36 years for an average, and then also the trial data from
#  the Cliflo data set, but I need to rerun this when I have downloaded more Cliflo data
m_m_rainfall <- vcn_weather %>% 
  filter(between(date, as.Date("1980-06-01"), as.Date("2017-05-31"))) %>%
  group_by(month, year) %>%
  summarise(month_rain = sum(rain)) %>%
  summarise(ave_month_rain = mean(month_rain))

trial_rainfall <- cliflo_weather %>%
  filter(between(date, as.Date("2017-06-01"), today())) %>%
  group_by(m, y) %>%
  summarise(mon_rainfall = sum(rain))


# #-----For Assignment 3######--------------
# # Autumn herd
# filter(vcn_weather, date == "2019-05-15")
# filter(vcn_weather, date == "2019-05-14")
# 
# summarise(vcn_weather, max(wind_run))
# 
# # Spring herd
# filter(vcn_weather, date == "2018-09-06")
# filter(vcn_weather, date == "2018-09-05")

#### Graphing the weather data over seasons instead of months #####

vcn_weather
# Need to pivot longer

vcn_weather <- vcn_weather %>% pivot_longer(cols = max_temp:evapo_trans,
                             names_to = "my_key_1",
                             values_to = "my_value")

# Add in my periods
p1 <- interval("1980-01-01", "1985-12-31")
p2 <- interval("1986-01-01", "1991-12-31")
p3 <- interval("1992-01-01", "1997-12-31")
p4 <- interval("1998-01-01", "2003-12-31")
p5 <- interval("2004-01-01", "2009-12-31")
p6 <- interval("2010-01-01", "2016-12-31")

vcn_weather <- mutate(vcn_weather,
                   my_period = if_else(vcn_weather$date %within% p1, "1",
                                       if_else(vcn_weather$date %within% p2, "2",
                                               if_else(vcn_weather$date %within% p3, "3",
                                                       if_else(vcn_weather$date %within% p4, "4",
                                                               if_else(vcn_weather$date %within% p5, "5",
                                                                       if_else(vcn_weather$date %within% p6, "6",
                                                                               "NA")))))))

# Second lot of periods, this one is ten years
y1 <- interval("1980-01-01", "1989-12-31")
y2 <- interval("1990-01-01", "1999-12-31")
y3 <- interval("2000-01-01", "2009-12-31")
y4 <- interval("2010-01-01", "2018-12-31")

vcn_weather <- vcn_weather %>% mutate(my_year = if_else(vcn_weather$date %within% y1, "1",
                                         if_else(vcn_weather$date %within% y2, "2",
                                                 if_else(vcn_weather$date %within% y3, "3",
                                                         if_else(vcn_weather$date %within% y4, "4",
                                                                 "NA")))))
    

vcn_summ <- vcn_weather %>% group_by(year, season, my_key_1) %>% 
  summarise(my_sum = sum(my_value)) %>% 
  ungroup()

# List for pairwise analysis

my_comparisons <- list(c("1", "2"),
                       c("1", "3"),
                       c("1", "4"),
                       c("2", "3"),
                       c("2", "4"),
                       c("3", "4"))

ggplot(data = filter(vcn_weather,
                     my_key_1 == "min_temp",
                     my_value > -100),
       aes(x = my_year,
           y = my_value,
           group = my_year)) +
  geom_boxplot() +
  stat_compare_means() +
  stat_summary(fun.y = mean, colour = "darkred", geom = "point", size = 3, show.legend = FALSE) +
  stat_summary(fun.y = mean, 
               colour = "red", 
               geom = "text", 
               show.legend = FALSE,
               vjust = -0.7, 
               aes(label = round(..y.., digits = 1))) +
  stat_compare_means(comparisons = my_comparisons,
                     label = "p.signif") +
                     # label.y = 0,
                     # position = position_dodge(width = 3/4)) +
  facet_wrap(.~season) 
  # coord_cartesian(ylim = c(0, 30))



## ----- New climate data analysis 24th Jan 2020 ####
# need to update the cliflo data



## plot of the monthly rainfall over the experiment against the evapotrans
vcn_weather1 <- read_csv("kavanagh_weather.csv",
                         col_types = cols(day = col_date(format = "%d/%m/%Y")))
vcn_weather1 <- select(vcn_weather1,
                       day:evapo_trans)

vcn_weather1 <- vcn_weather1 %>% pivot_longer(cols = max_temp:evapo_trans,
                              names_to = "my_key_1",
                              values_to = "my_value")

vcn_weather1 <- vcn_weather1 %>% mutate(my_day = day(day),
                                        my_month = month(day, label = F),
                                        my_year = year(day))





experiment_vcn <- vcn_weather1 %>% filter(day %within% interval("2017-06-01", "2020-01-31"))

# Summarising the experiment_vcn data
experiment_vcn_sum <- experiment_vcn %>% 
  filter(my_value > -200) %>% 
  group_by(my_year, my_month, my_key_1) %>% 
  summarise(my_sum = sum(my_value)) %>% 
  group_by(my_year, my_month, my_key_1) %>% 
  summarise(my_mean = mean(my_sum)) %>% 
  filter(my_key_1 %in% c("evapo_trans",
                   "rain")) %>%
  ungroup()


# Plot of the evapotranspiration and rainfall for each month over the trial period
ggplot(filter(experiment_vcn_sum,
              my_key_1 %in% c("evapo_trans",
                              "rain")),
       aes(x = my_month,
           y = my_mean)) +
  geom_col(aes(fill = my_key_1),
           position = "dodge") +
  facet_grid(.~my_year) +
  coord_cartesian(ylim = c(0, 300))



#### ------------- Climate analysis for the RESULTS 10th Feb 2020 ####

## Assigning a new period (8 yearly so I have five groups)

vcn_weather <- vcn_weather %>%
  mutate(eight_year_period = cut(date,
                           breaks = seq(ymd("1980-01-01"), ymd("2020-01-01"), '8 years'), 
                           labels = c("period_1", "period_2", "period_3", "period_4", "period_5")))

## Need to show a boxplot of over the 5 periods, changes in seasonal averages of 
#   temp and evapotrans

# Temperature is just averages, so no need for initial monthly summaries.

# Change the order of the season factors to get it as summer, autumn, winter, spring
vcn_weather$season <- factor(vcn_weather$season,
                             levels = c("summer", "autumn", "winter", "spring"))

# Set the list of pairwise analysis
my_period_comparisons <- list( c("period_1", "period_5"),
                               c("period_2", "period_5"),
                               c("period_3", "period_5"),
                               c("period_4", "period_5"),
                               c("period_1", "period_4"),
                               c("period_2", "period_4"),
                               c("period_3", "period_4"),
                               c("period_2", "period_3"),
                               c("period_1", "period_3"),
                               c("period_1", "period_2"))

# List for changing facet label names
seasonal_labels <- c(summer = "Summer",
                     autumn = "Autumn",
                     winter = "Winter",
                     spring = "Spring")
                                
                             

seasonal_max_temp_plot <- ggplot(filter(vcn_weather,
              my_key_1 == "max_temp",
              !is.na(eight_year_period)),
       aes(x = eight_year_period,
           y = my_value)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30),
                     name = expression("Seasonal average maximum temperature "~(degree ~ C))) +
  scale_x_discrete(name = "Observation period",
                   labels = c(period_1 = "1980 - 1987",
                              period_2 = "1988 - 1995",
                              period_3 = "1996 - 2003",
                              period_4 = "2004 - 2011",
                              period_5 = "2012 - 2019")) +
  stat_compare_means(comparisons = my_period_comparisons,
                     label = "p.signif",
                     method = "wilcox.test") +
  stat_summary(fun.y = mean,
               colour = "black",
               geom = "text", 
               size = 5,
               vjust = 5,
               aes(label = round(..y.., digits = 1)),
               show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 60)) +
  facet_wrap(season~.,
             labeller = labeller(season = seasonal_labels)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 20)) # Have to rewrite “Times” as the copy and paste doesn’t # make it go green


ggsave("Seasonal max temp plot.png",
       plot = seasonal_max_temp_plot,
       dpi = 300,
       width = 15,
       height = 10)



#### Linear regression of temperature per season over time ####

vcn_weather %>% filter(
  my_key_1 == "max_temp")

# Plot of every single point
ggplot(vcn_weather,
       aes(x = date,
           y = my_value)) +
  geom_smooth(method = "lm",
              formula = y ~ x) +
  facet_grid(my_key_1~season,
             scales = "free_y") +
  theme_classic() +
  stat_cor() +
  stat_regline_equation(vjust = 2)


# Averaging the yearly season first
weather_labels <- c(evapo_trans = "Evapotranspiration",
                    max_temp = "Maximum temperature",
                    min_temp = "Minimum temperature",
                    rain = "Rainfall")

seasonal_averages_plot <- vcn_weather %>% filter(my_key_1 %in% c("evapo_trans",
                                       "max_temp",
                                       "min_temp",
                                       "rain")) %>% 
  group_by(year, season, my_key_1) %>% 
  summarise(my_average = mean(my_value)) %>% 
  ggplot(aes(x = as.numeric(year),
             y = my_average)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_grid(my_key_1~season,
             scales = "free_y",
             labeller = labeller(season = seasonal_labels,
                                 my_key_1 = weather_labels)) +
  stat_cor(label.y.npc = "bottom",
           label.x.npc = "centre") +
  stat_regline_equation(label.y.npc = "bottom") +
  labs(x = "Year",
       y = "Seasonal daily average") +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(size = 18))


ggsave("Seasonal averages plot.png",
       plot = seasonal_averages_plot,
       dpi = 300,
       width = 19,
       height = 12)



# Sum of seasonal rainfall
vcn_weather %>% filter(my_key_1 %in% c("max_temp",
                                       "min_temp")) %>% 
  group_by(year, month, my_key_1) %>% 
  summarise(my_average = mean(my_value)) %>% 
  ggplot(aes(x = as.numeric(year),
             y = my_average)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_grid(month~my_key_1,
             scales = "free_y",
             labeller = labeller(season = seasonal_labels,
                                 my_key_1 = weather_labels)) +
  stat_cor(label.y.npc = "bottom",
           label.x.npc = "centre") +
  stat_regline_equation(label.y.npc = "bottom") +
  labs(x = "Year",
       y = "Seasonal daily average") +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(size = 18))


# ---------- Accounting for time series ####
# From
#  https://www.analyticsvidhya.com/blog/2015/12/complete-tutorial-time-series-modeling/

# Make a new dataframe that is just autumn minimum temperatures
autumn_min_temp <- vcn_weather %>% 
  filter(season == "autumn",
         my_key_1 == "min_temp",
         !is.na(my_value),
         my_value >= 0)

adf.test(diff(log(autumn_min_temp$my_value)))

acf(log(autumn_min_temp$my_value),
    ylim = c(-0.1, .1))

#### DO NEXT WEEK ####
# Potential website to follow https://cran.rstudio.com/web/packages/sweep/vignettes/SW01_Forecasting_Time_Series_Groups.html



#### Back to normal plotting of results ####
## Minimum temperature plots
seasonal_min_temp_plot <- ggplot(filter(vcn_weather,
                                        my_key_1 == "min_temp",
                                        !is.na(eight_year_period)),
                                 aes(x = eight_year_period,
                                     y = my_value)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(breaks = c(-15, -10, -5, 0, 5, 10, 15, 20),
                     name = expression("Seasonal average daily minimum air temperature "~(degree ~ C))) +
  scale_x_discrete(name = "Observation period",
                   labels = c(period_1 = "1980 - 1987",
                              period_2 = "1988 - 1995",
                              period_3 = "1996 - 2003",
                              period_4 = "2004 - 2011",
                              period_5 = "2012 - 2019")) +
  stat_compare_means(comparisons = my_period_comparisons,
                     label = "p.signif") +
  stat_summary(fun.y = mean,
               colour = "black",
               geom = "text", 
               size = 5,
               vjust = 5,
               aes(label = round(..y.., digits = 1)),
               show.legend = FALSE) +
  coord_cartesian(ylim = c(-10, 55)) +
  facet_wrap(season~.,
             labeller = labeller(season = seasonal_labels)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 20)) # Have to rewrite “Times” as the copy and paste doesn’t # make it go green


ggsave("Seasonal min temp plot.png",
       plot = seasonal_min_temp_plot,
       dpi = 300,
       width = 15,
       height = 10)


## Evapotranspiration plots
# Firstly need to group into monthly summaries
monthly_evapo_trans <- vcn_weather %>% 
  filter(my_key_1 == "evapo_trans",
         !is.na(eight_year_period)) %>% 
  group_by(season, eight_year_period, year, month) %>% 
  summarise(evapo_trans_month = sum(my_value)) %>% 
  ungroup()

# Monthly summarised evapotrans
ggplot(monthly_evapo_trans,
                                 aes(x = eight_year_period,
                                     y = evapo_trans_month)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(breaks = c(0, 50, 100, 150),
                     name = expression("Seasonal average daily minimum air temperature "~(degree ~ C))) +
  scale_x_discrete(name = "Observation period",
                   labels = c(period_1 = "1980 - 1987",
                              period_2 = "1988 - 1995",
                              period_3 = "1996 - 2003",
                              period_4 = "2004 - 2011",
                              period_5 = "2012 - 2019")) +
  stat_compare_means(comparisons = my_period_comparisons,
                     label = "p.signif") +
  stat_summary(fun.y = mean,
               colour = "black",
               geom = "text", 
               size = 5,
               vjust = 5,
               aes(label = round(..y.., digits = 1)),
               show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 350)) +
  facet_wrap(season~.,
             labeller = labeller(season = seasonal_labels)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 20)) # Have to rewrite “Times” as the copy and paste doesn’t # make it go green


# Daily evapotrans plot
ggplot(filter(vcn_weather,
              my_key_1 == "evapo_trans",
              !is.na(eight_year_period)),
       aes(x = eight_year_period,
           y = my_value)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(breaks = c(-15, -10, -5, 0, 5, 10, 15, 20),
                     name = expression("Seasonal average daily minimum air temperature "~(degree ~ C))) +
  scale_x_discrete(name = "Observation period",
                   labels = c(period_1 = "1980 - 1987",
                              period_2 = "1988 - 1995",
                              period_3 = "1996 - 2003",
                              period_4 = "2004 - 2011",
                              period_5 = "2012 - 2019")) +
  stat_compare_means(comparisons = my_period_comparisons,
                     label = "p.signif") +
  stat_summary(fun.y = mean,
               colour = "black",
               geom = "text", 
               size = 5,
               vjust = 5,
               aes(label = round(..y.., digits = 1)),
               show.legend = FALSE) +
  coord_cartesian(ylim = c(-10, 55)) +
  facet_wrap(season~.,
             labeller = labeller(season = seasonal_labels)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 20))







#### Data analysis for the methods ####

# Average monthly rainfall, evapotranspiration
vcn_weather1 %>%  filter(my_key_1 %in%  c("evapo_trans", "rain")) %>% 
  group_by(my_year, my_month, my_key_1) %>% 
  summarise(my_sum = sum(my_value)) %>% 
  group_by(my_month, my_key_1) %>% 
  summarise(my_mean = mean(my_sum)) %>% 
  pivot_wider(names_from = my_key_1,
              values_from = my_mean) %>% 
  flextable() %>% 
  colformat_num(digits = 0,
                col_keys = c("evapo_trans",
                             "rain")) %>% 
  set_header_labels("evapo_trans" = "Evapotranspiration",
                    "rain" = "Rainfall",
                    "my_month" = "Month") %>% 
  autofit()

# Rainfall and evapotrans graphs

rainfall_evapotrans_ave <- vcn_weather1 %>%  filter(my_key_1 %in%  c("evapo_trans", "rain")) %>% 
  group_by(my_year, my_month, my_key_1) %>% 
  summarise(my_sum = sum(my_value)) %>% 
  group_by(my_month, my_key_1) %>% 
  summarise(my_mean = mean(my_sum)) 

rainfall_evapotrans_ave <- rainfall_evapotrans_ave %>% 
  mutate(my_month2 = month(my_month, label = T))
 

# Plot of the average rainfall and evapotranspiration
ggplot(rainfall_evapotrans_ave) +
  geom_col(aes(x = my_month2,
               y = my_mean,
               fill = my_key_1),
           position = "dodge")  +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 11)) + # Have to rewrite “Times” as the copy and paste doesn’t # make it go green
  labs(x = "",
       y = "mm") +
  scale_fill_manual(name = "Monthly average (1980-2016)",
                    values = c("tomato4",
                               "royalblue2"),
                    labels = c("Evapotranspiration",
                               "Rainfall"))


# Mean max and min temp
vcn_weather1 %>%  filter(my_key_1 %in%  c("max_temp", "min_temp")) %>% 
  group_by(my_year, my_month, my_key_1) %>% 
  summarise(my_mean1 = mean(my_value)) %>% 
  group_by(my_month, my_key_1) %>% 
  summarise(my_mean2 = mean(my_mean1)) %>% 
  pivot_wider(names_from = my_key_1,
              values_from = my_mean2) %>% 
  flextable() %>% 
  colformat_num(digits = 0,
                col_keys = c("max_temp",
                             "min_temp")) %>% 
  set_header_labels("max_temp" = "Max temperature",
                    "min_temp" = "Min temperature",
                    "my_month" = "Month") %>% 
  autofit()


# Graph for temperature
vcn_weather1 %>%  filter(my_key_1 %in%  c("max_temp", "min_temp")) %>% 
  group_by(my_year, my_month, my_key_1) %>% 
  summarise(my_mean1 = mean(my_value)) %>% 
  group_by(my_month, my_key_1) %>% 
  summarise(my_mean2 = mean(my_mean1)) %>% 
  ggplot() +
  geom_col(aes(x = my_month,
               y = my_mean2),
           fill = "grey") +
  facet_grid(.~my_key_1) +
  theme_pubr()


## Plot of experiment weather vs average

# Dataframe for averages
vcn_ave <- vcn_weather1 %>%  filter(my_key_1 %in%  c("evapo_trans", "rain"),
                                    my_value > -100) %>% 
  group_by(my_year, my_month, my_key_1) %>% 
  summarise(my_sum = sum(my_value)) %>% 
  group_by(my_month, my_key_1) %>% 
  summarise(my_mean = mean(my_sum)) %>% 
  mutate(source = "long_term_ave")

# Make three new frames to add in the different years

vcn_ave1 <- vcn_ave %>% mutate(my_year = 2017)
vcn_ave2 <- vcn_ave %>% mutate(my_year = 2018)
vcn_ave3 <- vcn_ave %>% mutate(my_year = 2019)
vcn_ave4 <- vcn_ave %>% mutate(my_year = 2020)

joined_vcn <- bind_rows(vcn_ave1,
          vcn_ave2,
          vcn_ave3,
          vcn_ave4)

# Dataframe for experimental period
trial_ave <- vcn_weather1 %>%  filter(my_key_1 %in%  c("evapo_trans", "rain"),
                                      day %within% interval("2017-06-01", today()),
                                      my_value > -100) %>% 
  group_by(my_year, my_month, my_key_1) %>% 
  summarise(my_mean = sum(my_value)) %>%
  mutate(source = "experimental_period") %>% 
  select(my_month,
         my_key_1,
         my_mean,
         source,
         my_year)

weather_joined <- bind_rows(trial_ave,
          joined_vcn)


# Make a new column of dates so that I can then filter it out later on

weather_joined <- weather_joined %>% mutate(my_day = "01",
  my_date = str_c(my_year, my_month, my_day, sep = "-"),
  test_date = ymd(my_date)) %>% 
  filter(test_date %within% interval(ymd("2017-06-01"), ymd("2020-01-31")))




weather_names <- c(
  evapo_trans = "Evapotranspiration",
  rain = "Rainfall"
)


#### Plot of the evapotrans and rainfall for the experimental period compared to averages #####
rain_evapotrans_experiment <- ggplot(weather_joined) +
  geom_col(aes(x = test_date,
               y = my_mean,
               fill = source),
           position = position_dodge(),
           width = 20) +
  facet_grid(my_key_1~.,
             labeller = labeller(my_key_1 = weather_names)) +
  scale_fill_manual(values = c("grey0",
                               "grey70"),
                    name = "Data",
                    labels = c("Experimental period",
                               "Long term average (1980 - 2020)")) +
  scale_x_date(breaks = "2 months",
               date_labels = "%b-%Y") +
  scale_y_continuous(breaks = c(seq(0, 200, 25))) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 15)) + # Have to rewrite “Times” as the copy and paste doesn’t # make it go green

  labs(y = "mm",
       x = "")

ggsave("Rainfall and evapotrans over the experimental period.png",
       plot = rain_evapotrans_experiment,
       dpi = 300,
       width = 17,
       height = 6)

#### Farmers forum Plot of the rainfall over the experimental period ####
rain_experiment <- ggplot(filter(weather_joined,
                                 my_key_1 == "rain")) +
  geom_col(aes(x = test_date,
               y = my_mean,
               fill = source),
           position = position_dodge(),
           width = 20) +
  # facet_grid(my_key_1~.,
  #            labeller = labeller(my_key_1 = weather_names)) +
  scale_fill_manual(values = c("gray70",
                               "gray32"),
                    name = "Data",
                    labels = c("Experimental period",
                               "Long term average\n(1980 - 2020)")) +
  scale_x_date(breaks = c(as.Date("2017-06-01"),
                               as.Date("2017-12-01"),
                               as.Date("2018-06-01"),
                               as.Date("2018-12-01"),
                               as.Date("2019-06-01"),
                               as.Date("2019-12-01")),
               date_labels = "%b-%Y") +
  scale_y_continuous(breaks = c(seq(0, 200, 25)),
                     expand = expand_scale(0,0)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(size = 30),
        axis.ticks.length = unit(0.25, "cm")) +
  labs(y = "mm",
       x = "")

ggsave("FF Rainfall over the experimental period.png",
       plot = rain_experiment,
       dpi = 300,
       width = 21,
       height = 8)



#### Farmers Forum Evapotranspiraion over the experiment period ####
evapotrans_experiment <- ggplot(filter(weather_joined,
                                 my_key_1 == "evapo_trans")) +
  geom_col(aes(x = test_date,
               y = my_mean,
               fill = source),
           position = position_dodge(),
           width = 20) +
  # facet_grid(my_key_1~.,
  #            labeller = labeller(my_key_1 = weather_names)) +
  scale_fill_manual(values = c("grey0",
                               "grey70"),
                    name = "Data",
                    labels = c("Experimental period",
                               "Long term average\n (1980 - 2020)")) +
  scale_x_date(breaks = "2 months",
               date_labels = "%b-%Y") +
  scale_y_continuous(breaks = c(seq(0, 200, 25))) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 30),
        axis.text.x = element_text(angle = 90)) + # Have to rewrite “Times” as the copy and paste doesn’t # make it go green
  
  labs(y = "mm",
       x = "")

ggsave("FF Evapotrans over the experimental period.png",
       plot = evapotrans_experiment,
       dpi = 300,
       width = 21,
       height = 8)

#### RESULTS Evapotranspiraion over the experiment period ####
evapotrans_experiment <- ggplot(filter(weather_joined,
                                       my_key_1 == "evapo_trans")) +
  geom_col(aes(x = test_date,
               y = my_mean,
               fill = source),
           position = position_dodge(),
           width = 20) +
  # facet_grid(my_key_1~.,
  #            labeller = labeller(my_key_1 = weather_names)) +
  scale_fill_manual(values = c("grey0",
                               "grey70"),
                    name = "Data",
                    labels = c("Experimental period",
                               "Long term average\n (1980 - 2020)")) +
  scale_x_date(breaks = "2 months",
               date_labels = "%b-%Y") +
  scale_y_continuous(breaks = c(seq(0, 200, 25))) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 30),
        axis.text.x = element_text(angle = 90)) + # Have to rewrite “Times” as the copy and paste doesn’t # make it go green
  
  labs(y = "mm",
       x = "")

ggsave("FF Evapotrans over the experimental period.png",
       plot = evapotrans_experiment,
       dpi = 300,
       width = 21,
       height = 8)



# Practise with the egg package and monthly values from date
rainfall_evapotrans_plot <- ggplot(weather_joined) +
  geom_col(aes(x = test_date,
               y = my_mean,
               fill = source),
           position = position_dodge()) +
  facet_grid(my_key_1~.,
             labeller = labeller(my_key_1 = weather_names)) +
  scale_fill_manual(values = c("grey",
                               "grey22")) +
  scale_x_date(date_breaks = "2 months",
               labels = date_format("%m/%Y")) +
  labs(y = "mm",
       x = "") +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 11)) # Have to rewrite “Times” as the copy and paste doesn’t # make it go green


tag_facet_outside(rainfall_evapotrans_plot)
  




#### _______ Time series analysis of temperature over time _________ ####
          
    # New df of just date and min temp

weather
      

min_temp_data <- vcn_weather %>% filter(my_key_1 == "min_temp",
                                        case_when(month == "02" ~ day != "29",
                                                  month != "02" ~ T))
      
winter_min_temp <- min_temp_data %>% 
  filter(season == "winter") %>% 
  group_by(year) %>% 
  summarise(ave_winter_temp = mean(my_value)) %>% 
  select(ave_winter_temp)

      
# Covert to a time series

winter_time_series <- ts(winter_min_temp, frequency = 92)

stl(winter_time_series[, 1])

dim(winter_time_series)

plot(winter_time_series)

# This works
daily_min_temp <- ts(select(min_temp_data, my_value), start = c(1980, 6, 1), frequency = 365)

stl(daily_min_temp[, 1], s.window = "period")

components.ts <- decompose(daily_min_temp)

plot(components.ts)

winter_daily_temp <- min_temp_data %>% filter(my_key_1 == "min_temp",
                                              season == "winter",
                                              case_when(month == "02" ~ day != "29",
                                                        month != "02" ~ T)) %>% 
                                select(my_value)

winter_daily_ts <- ts(winter_daily_temp, frequency = 92)

components.ts2 <- decompose(winter_daily_ts)

plot(components.ts2)

acf(daily_min_temp, lag.max = 365)

acf(winter_daily_ts, lag.max = 10)



max_temps <- vcn_weather %>% filter(my_key_1 == "max_temp",
                                    case_when(month == "02" ~ day != "29",
                                              month != "02" ~ T)) %>% 
  select(my_value)

max_temps_ts <- ts(max_temps, start = c(1980, 1, 1), frequency = 365)

components.ts3 <- decompose(max_temps_ts)

plot(components.ts3)

evapo_trans <- vcn_weather %>% filter(my_key_1 == "evapo_trans",
                                      case_when(month == "02" ~ day != "29",
                                                month != "02" ~ T)) %>% 
  select(my_value)

evapo_trans_ts <- ts(evapo_trans, start = c(1980, 1, 1), frequency = 365)

components.ts_evapo_trans <- decompose(evapo_trans_ts)

plot(components.ts_evapo_trans)

components.ts2$trend
plot(components.ts2$trend)

# Turn the trend into a data frame
min_temp_trend <- as.data.frame(components.ts2$trend) %>% 
  rownames_to_column() %>% 
  mutate(dummy_date = seq.Date(from = as.Date("1980-01-01"),
                               by = 1,
                                length.out = dim(min_temp_trend)[1]))

ggplot(min_temp_trend,
       aes(y = V1,
           x = dummy_date)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(label.y = 7) +
  stat_regline_equation(label.y = 6.5)



# Using the stl function
evpo_trans_stl <- stl(evapo_trans_ts[, 1], s.window = "period")

plot(evpo_trans_stl)


min_temp_stl <- stl(daily_min_temp[, 1], s.window = "period")

plot(min_temp_stl)


max_temp_stl <- stl(max_temps_ts[, 1], s.window = "period")

plot(max_temp_stl)


seasonal_min_temp_plot
seasonal_averages_plot



components.ts$trend


test <- as.data.frame(components.ts$trend) %>% 
  rownames_to_column()

ggplot(test, aes(x = as.numeric(rowname),
                 y = V1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(label.y = 10) +
  stat_regline_equation(label.y = 9.5)



#### ___________ Final RESULTS ####

# Make a dataframe that is just the min temps
# Don't filter out the leap year days
final_min_temp <- vcn_weather %>% 
  filter(my_key_1 == "min_temp",
         date <= as.Date("2019-12-31")) %>% 
         # case_when(month == "02" ~ day != "29",
         #           month != "02" ~ T)) %>% 
  select(my_value)

# Convert the dataframe to a time series graph
# Set frequency to 365.25 to account for leap years, which works well
final_min_temp_ts <- ts(final_min_temp, start = c(1980, 1), frequency = 365.25)


# Use the stl function to generate the seasonal, trend and random error
final_min_temp_stl <- stl(final_min_temp_ts[, 1], s.window = "period")


# Plot of the four sections of it, data, seasonal, trend  and random error
plot(final_min_temp_stl)


# Can extract the trend values by specifying the second column of the time.series data
final_min_temp_stl$time.series[,2]

# Turn the trend values into a dataframe to then ggplot
final_min_temp_trend <- as.data.frame(final_min_temp_stl$time.series[,2]) %>% 
  mutate(dummy_day = seq.Date(from = as.Date("1980-01-01"),
                                       by = 1,
                                       length.out = 14606),
         x = as.numeric(x))


# Final plot of min temp trend from stl function
rr_daily_min_temp <- ggplot(final_min_temp_trend,
       aes(x = dummy_day,
           y = x)) +
  geom_point(size = 1,
             colour = "gray50") +
  geom_smooth(method = "lm",
              colour = "black",
              linetype = "dashed",
              size = 2.5) +
  stat_cor(aes(label = paste(..rr.label.., 
                             "p<0.001", 
                             sep = "~`,`~")), # have to hard code the p value in
           label.y = 5,
           size = 8,
           family = "Times") +
  stat_regline_equation(label.y = 6,
                        size = 8,
                        family = "Times") +
  coord_cartesian(ylim = c(0, 10)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 22,
                            colour = "black"),
        axis.text = element_text(family = "Times",
                            size = 22,
                            colour = "black"),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_x_date(breaks = seq(as.Date("1980-01-01"), 
                            as.Date("2021-01-01"), 
                            by = "5 years"),
               date_labels = "%Y") +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(y = "Seasonally-adjusted daily minimum air temperature (°C)",
       x = "Date")

ggsave("RR daily min temp.png",
       plot = rr_daily_min_temp,
       dpi = 600,
       width = 12,
       height = 8)



 ### Extracting the standard error of the temp increase ##

final_min_temp_trend

min_temp_lm <- lm(x ~ dummy_day, data = final_min_temp_trend)

#
# Standard error of the daily min temp linear model, mutiplied by 39 years
summary.lm(min_temp_lm)$coefficients[2,1] * 365 * 39
summary.lm(min_temp_lm)$coefficients[2,2] *365 *39

# For the max temp
max_temp_lm <- lm(x ~ dummy_day, data = final_max_temp_trend)
summary.lm(max_temp_lm)$coefficients[2,1] * 365 *39
summary.lm(max_temp_lm)$coefficients[2,2] * 365 *39



    ## Second attempt at just the winter values for min temp ###


    min_temp_winter <- vcn_weather %>% 
      filter(my_key_1 == "min_temp",
             season == "winter",
             date <= as.Date("2019-12-31")) %>% 
      # case_when(month == "02" ~ day != "29",
      #           month != "02" ~ T)) %>% 
      select(my_value)
    
    # Convert the dataframe to a time series graph
    # Set frequency to 92 days
    min_temp_winter_ts <- ts(min_temp_winter, frequency = 92)
    
    
    # Use the stl function to generate the seasonal, trend and random error
    min_temp_winter_stl <- stl(min_temp_winter_ts[, 1], s.window = "period")
    
    
    # Plot of the four sections of it, data, seasonal, trend  and random error
    plot(min_temp_winter_stl)
    
    
    # Can extract the trend values by specifying the second column of the time.series data
    min_temp_winter_stl$time.series[,2]
    
    # Turn the trend values into a dataframe to then ggplot
    min_temp_winter_trend <- as.data.frame(min_temp_winter_stl$time.series[,2]) %>% 
      mutate(x = as.numeric(x)) %>% 
      rownames_to_column() %>% 
      rename(temp = x)
    
    min_temp_winter_trend$rowname <- as.numeric(min_temp_winter_trend$rowname)
    
    ## Plotting the trend
    rr_daily_min_temp_winter_plot <- ggplot(min_temp_winter_trend,
           aes(x = rowname,
               y = temp)) +
      geom_point(size = 1,
                 colour = "gray50") +
      geom_smooth(method = "lm",
                  colour = "black",
                  linetype = "dashed",
                  size = 2.5) +
      stat_cor(aes(label = paste(..rr.label.., "p<0.001", sep = "~`,`~")), # have to hard code the p value in
               label.y = 2,
               size = 8,
               family = "Times") +
      stat_regline_equation(label.y = 2.5,
                            size = 8,
                            family = "Times") +
      coord_cartesian(ylim = c(0, 10)) +
      theme(panel.background = element_blank(),
            axis.line = element_line(size = 1, linetype = "solid",
                                     colour = "black"),
            text = element_text(family = "Times",
                                size = 22),
            axis.text = element_text(colour = "black",
                                     size = 22,
                                     family = "Times"),
            axis.ticks.length = unit(0.25, "cm")) +
      scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
      scale_x_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500),
                         labels = comma) +
      labs(y = "Seasonally-adjusted winter daily minimum air temperature (°C)",
           x = "Winter days (June–August, inclusive) since 1 June 1980")
    
    
    ggsave("RR daily min temp winter.png",
           plot = rr_daily_min_temp_winter_plot,
           dpi = 600,
           width = 12,
           height = 9)
    
    
    
    # Calculating the increase per day with standard error
    min_temp_winter_lm <- lm(temp ~ rowname, data = min_temp_winter_trend)

    summary.lm(min_temp_winter_lm)$coefficients[2,1] * 92 * 39 # 39 year increase
    summary.lm(min_temp_winter_lm)$coefficients[2,2] * 92 *39 # standard error of increase







# Mutate a season column based on date to then just look at winter
# Seasons
season_df <- data.frame(month = as.numeric(c("12", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")), 
                     season = rep(c("summer", "autumn", "winter", "spring"), each = 3),
                     stringsAsFactors = FALSE)

final_min_temp_trend <- final_min_temp_trend %>% 
  mutate(month = month(dummy_day))

min_temp_trend_w_season <- inner_join(final_min_temp_trend, season_df,
          by = "month")


ggplot(filter(min_temp_trend_w_season,
              season == "winter"),
       aes(x = dummy_day,
           y = x)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(label.y = 7) +
  stat_regline_equation(label.y = 6.5) +
  coord_cartesian(ylim = c(0, 10.5))


min_temp_row_names <- min_temp_trend_w_season %>% 
  rownames_to_column()

min_temp_row_names$rowname <- as.numeric(min_temp_row_names$rowname)

# Does not join data points up
ggplot(filter(min_temp_row_names,
              season == "winter"),
       aes(x = rowname,
           y = x)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(label.y = 7) +
  stat_regline_equation(label.y = 6.5) +
  coord_cartesian(ylim = c(0, 10.5))


# Need to filter out everything except winter then mutate new dummy column

winter_min_temp <- min_temp_trend_w_season %>% 
  filter(season == "winter") 


winter_min_temp <- winter_min_temp %>% 
  mutate(new_day = seq(1, length.out = dim(winter_min_temp)[1]))

# New plot of only winter values by a new dummy day column that connects 
#  the values
ggplot(winter_min_temp,
       aes(x = new_day,
           y = x)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(label.y = 7) +
  stat_regline_equation(label.y = 6.5) +
  coord_cartesian(ylim = c(0, 10.5))





#### ______ Max Temp ________ ####
# Make a dataframe that is just the min temps
# Don't filter out the leap year days
final_max_temp <- vcn_weather %>% 
  filter(my_key_1 == "max_temp",
         date <= as.Date("2019-12-31")) %>% 
  # case_when(month == "02" ~ day != "29",
  #           month != "02" ~ T)) %>% 
  select(my_value)

# Convert the dataframe to a time series graph
# Set frequency to 365.25 to account for leap years, which works well
final_max_temp_ts <- ts(final_max_temp, start = c(1980, 1), frequency = 365.25)


# Use the stl function to generate the seasonal, trend and random error
final_max_temp_stl <- stl(final_max_temp_ts[, 1], s.window = "period")


# Plot of the four sections of it, data, seasonal, trend  and random error
plot(final_max_temp_stl)


# Can extract the trend values by specifying the second column of the time.series data
final_max_temp_stl$time.series[,2]

# Turn the trend values into a dataframe to then ggplot
final_max_temp_trend <- as.data.frame(final_max_temp_stl$time.series[,2]) %>% 
  mutate(dummy_day = seq.Date(from = as.Date("1980-01-01"),
                              by = 1,
                              length.out = 14606),
         x = as.numeric(x))


# Plotting
rr_max_temp <- ggplot(final_max_temp_trend,
                      aes(x = dummy_day,
                          y = x)) +
  geom_point(size = 1,
             colour = "gray50") +
  geom_smooth(method = "lm",
              colour = "black",
              linetype = "dashed",
              size = 2.5) +
  stat_cor(aes(label = paste(..rr.label.., "p<0.001", sep = "~`,`~")), # have to hard code the p value in
           label.y = 10,
           size = 8,
           family = "Times") +
  stat_regline_equation(label.y = 12,
                        size = 8,
                        family = "Times") +
  coord_cartesian(ylim = c(0, 20)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 22),
        axis.text = element_text(colour = "black",
                                 size = 22,
                                 family = "Times"),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_x_date(breaks = seq(as.Date("1980-01-01"), 
                            as.Date("2021-01-01"), 
                            by="5 years"),
               date_labels = "%Y") +
  labs(y = "Seasonally-adjusted daily maximum air temperature (°C)",
       x = "Date")


ggsave("RR daily max temp.png",
       plot = rr_max_temp,
       dpi = 600,
       width = 12,
       height = 8)


# Using R squared instead
summary(lm(x ~ dummy_day, data = final_max_temp_trend))




#### ______ Evapo trans ________ ####
# Make a dataframe that is just the min temps
# Don't filter out the leap year days
final_evapo_trans <- vcn_weather %>% 
  filter(my_key_1 == "evapo_trans",
         date <= as.Date("2019-12-31")) %>% 
  # case_when(month == "02" ~ day != "29",
  #           month != "02" ~ T)) %>% 
  select(my_value)

# Convert the dataframe to a time series graph
# Set frequency to 365.25 to account for leap years, which works well
final_evapo_trans_ts <- ts(final_evapo_trans, start = c(1980, 1), frequency = 365.25)


# Use the stl function to generate the seasonal, trend and random error
final_evapo_trans_stl <- stl(final_evapo_trans_ts[, 1], s.window = "period")


# Plot of the four sections of it, data, seasonal, trend  and random error
plot(final_evapo_trans_stl)


# Can extract the trend values by specifying the second column of the time.series data
final_evapo_trans_stl$time.series[,2]

# Turn the trend values into a dataframe to then ggplot
final_evapo_trans_trend <- as.data.frame(final_evapo_trans_stl$time.series[,2]) %>% 
  mutate(dummy_day = seq.Date(from = as.Date("1980-01-01"),
                              by = 1,
                              length.out = 14606),
         x = as.numeric(x))


# Plotting
ggplot(final_evapo_trans_trend,
       aes(x = dummy_day,
           y = x)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(label.y = 4.5) +
  stat_regline_equation(label.y = 4) +
  coord_cartesian(ylim = c(0, 5))



#### ______ Solar radiation ________ ####
# Make a dataframe that is just the solar radiation
# Don't filter out the leap year days
final_solar_radn <- vcn_weather %>% 
  filter(my_key_1 == "solar_radn",
         date <= as.Date("2019-12-31")) %>% 
  # case_when(month == "02" ~ day != "29",
  #           month != "02" ~ T)) %>% 
  select(my_value)

# Convert the dataframe to a time series graph
# Set frequency to 365.25 to account for leap years, which works well
final_solar_radn_ts <- ts(final_solar_radn, start = c(1980, 1), frequency = 365.25)


# Use the stl function to generate the seasonal, trend and random error
final_solar_radn_stl <- stl(final_solar_radn_ts[, 1], s.window = "period")


# Plot of the four sections of it, data, seasonal, trend  and random error
plot(final_solar_radn_stl)


# Can extract the trend values by specifying the second column of the time.series data
final_solar_radn_stl$time.series[,2]

# Turn the trend values into a dataframe to then ggplot
final_solar_radn_trend <- as.data.frame(final_solar_radn_stl$time.series[,2]) %>% 
  mutate(dummy_day = seq.Date(from = as.Date("1980-01-01"),
                              by = 1,
                              length.out = 14606),
         x = as.numeric(x))


# Plotting
ggplot(final_solar_radn_trend,
       aes(x = dummy_day,
           y = x)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(label.y = 15) +
  stat_regline_equation(label.y = 15.5) +
  coord_cartesian(ylim = c(0, 16))



#### ______ Water balance _____________ ####


water_balance <- weather %>% select(-max_temp:-wind_run) %>% 
  filter(day > as.Date("1980-05-31"))




    # Calculate the actual evapotranspiration
  
# Picture of the code, but can do it with if_else in mutate                
if(TAW >(0.5 * field_capacity)) {
  x = evapo_trans
} else {
  x = (TAW/field_capacity) * evapo_trans
}


water_balance %>% 
  mutate(actual_evapo_trans = if_else(
    lag(TAW) > (0.5 * field_capacity),
    evapo_trans,
    evapo_trans * (TAW/field_capacity)
  ))
          
               
view(water_balance %>% 
  mutate(c = c(k, k + cumsum(rain - evapo_trans)[-1])))
    
# Values from Soil Science book
# Number for the maximum mm of the soil
field_capacity <- 44

# Number for the minimum
wilting_point <- 24

# number for the stress point (arbitary?)
# 40% of the avaliable water??
stress_point <- 32



view(water_balance %>% 
  mutate(my_field_cap = field_capacity,
         my_stress_point = stress_point,
         my_wilting_point = wilting_point,
    actual_et = evapo_trans,
         TAW = (field_capacity + rain - actual_et),
         actual_et = if_else(lag(TAW) > stress_point,
                             evapo_trans,
                             (TAW/stress_point) * evapo_trans),
         TAW = if_else((lag(TAW) + rain - actual_et) > field_capacity,
                       field_capacity,
                       (lag(TAW) + rain - actual_et))))
    


    
    




# water_balance %>% mutate(TAW = case_when(
#   day == as.Date("1980-06-01") ~ field_capacity),
#   day != as.Date("1980-06-01") ~ case_when(
#     (lag(TAW) + rain - evapo_trans) >= field_capacity ~
#       field_capacity,
#     (lag(TAW) + rain - evapo_trans) <= field_capacity ~
#             (lag(TAW) + rain - evapo_trans)))
# 
# 
# 
# water_balance %>% mutate(TAW = case_when(
#   day != as.Date("1980-06-01") ~ case_when(
#     (lag(TAW) + rain - evapo_trans) >= field_capacity ~
#       field_capacity,
#     (lag(TAW) + rain - evapo_trans) <= field_capacity ~
#       (lag(TAW) + rain - evapo_trans))))
# 
# water_balance %>% mutate(my_field_capacity = field_capacity,
#                          my_wilting_point = wilting_point,
#                          TAW = my_field_capacity + rain - evapo_trans)
# 
# 
# water_balance %>% mutate(TAW1 =
#                            case_when(
#                              day == as.Date("1980-06-01") ~ 
#                                field_capacity + rain - evapo_trans)) %>% 
#   mutate(TAW2 = case_when(day != as.Date("1980-06-01") ~ (lag(TAW1) + rain - evapo_trans)))


# water_balance %>% mutate(water_holding = case_when(
#   day == as.Date("1980-06-01") ~ field_capacity,
#   day != as.Date("1980-06-01") ~ lag(cumsum(field_capacity - water_holding)))


# water_balance <- water_balance %>% mutate(TAW = case_when(
#   day == as.Date("1980-06-01") ~ field_capacity))
# 
# water_balance %>% mutate(TAW2 = case_when(
#   day == as.Date("1980-06-02") ~ lag(TAW)))


# # https://stackoverflow.com/questions/58349410/generating-series-from-lagging-data-with-mutate-verb-in-r-tidyverse
# 
# water_balance %>%
#   mutate(fc = field_capacity) %>% 
#   mutate(one_love = {
#     one <- fc[1]
#     for(i in seq(2, NROW(fc))) {
#       one[i] = (one[i-1] + rain - evapo_trans)
#     }
#     one
#   })
# 
# warnings()

k <- 44
water_balance %>% 
  mutate(wet_soil = case_when(
    row_number() == 1 ~ k,
    row_number() != 1 ~ lag(wet_soil) + rain - evapo_trans))


water_balance %>% 
  mutate(c = c(k, k + cumsum(rain - evapo_trans)[-1]))



 #### CHEATING ####

water_balance2 <- read_csv("water balance with TAW.csv",
         col_types = cols(day = col_date(format = "%d/%m/%Y")))

# water_balance2 <- water_balance2 %>% 
#   rename(actual_et = `Actual ET`)


water_balance2 <- left_join(water_balance2, season_df, by = "month")


field_capacity2 <- 80
stress_point2 <- 32
wilting_point2 <- 0

water_balance2 <- water_balance2 %>% 
  mutate(dry_day = if_else(TAW < stress_point2,
                           T, F))

water_balance2 %>% 
  group_by(year, season) %>% 
  summarise(how_many = sum(dry_day)) %>% 
  ggplot(aes(x = year, 
             y = how_many)) +
  geom_point() +
  facet_wrap(.~season)



ggplot(water_balance2,
       aes(x = day,
           y = TAW)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_regline_equation()

lm(TAW ~ day, data = water_balance2)


water_balance2 <- water_balance2 %>% 
  mutate(last_day_above_stress_point = if_else(dry_day == F, day, as.Date(NA)))%>%
  fill(last_day_above_stress_point) %>%
    mutate(days_since_surplus = day - last_day_above_stress_point)


water_balance2 %>% 
  group_by(year, season) %>% 
  summarise(ave_days_dry = mean(days_since_surplus, na.rm = T)) %>% 
  ggplot(aes(x = year,
             y = ave_days_dry)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_regline_equation() +
  stat_cor(label.y = 30) +
  facet_wrap(.~season)



ggplot(water_balance2,
       aes(x = day,
           y = days_since_surplus)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(.~season)

## Need to pluck out the max value from each continuous section of T and F

water_balance2 <- water_balance2 %>% 
  group_by(dry_day, grp = with(rle(dry_day), 
                               rep(seq_along(lengths), 
                                   lengths))) %>% 
  mutate(my_seq = seq_along(grp)) %>% 
    group_by(grp) %>% 
    mutate(max_for_grp = if_else(dry_day == T,
                                 max(my_seq),
                                 NA_integer_)) %>% 
    ungroup()

# Bad because its plotting the multiple "72" days in summer as multiple points
# Need it as just one number
ggplot(water_balance2,
       aes(x = day,
           y = max_for_grp)) +
  geom_jitter() +
  facet_wrap(.~season)


water_balance2 <- water_balance2 %>% group_by(grp) %>% 
  mutate(last_day_before_water = if_else(dry_day == T,
                                         max(day),
                                         as.Date(NA))) %>% 
           ungroup()

days_under_stress_point <- water_balance2 %>% select(season, year, max_for_grp, last_day_before_water) %>% 
  distinct()

ggplot(days_under_stress_point,
       aes(x = last_day_before_water,
           y = max_for_grp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_regline_equation() +
  stat_cor(label.y = 50) +
  facet_wrap(.~season)

#### ________ Minimum temp but from 1988 onwards _ ####


final_min_temp_1988 <- vcn_weather %>% 
  filter(my_key_1 == "min_temp",
         date <= as.Date("2019-12-31"),
         date >= as.Date("1988-01-01")) %>% 
   select(my_value)

# Convert the dataframe to a time series graph
# Set frequency to 365.25 to account for leap years, which works well
final_min_temp_ts_1988 <- ts(final_min_temp_1988, start = c(1980, 1), frequency = 365.25)


# Use the stl function to generate the seasonal, trend and random error
final_min_temp_stl_1988 <- stl(final_min_temp_ts_1988[, 1], s.window = "period")


# Plot of the four sections of it, data, seasonal, trend  and random error
plot(final_min_temp_stl_1988)


# Can extract the trend values by specifying the second column of the time.series data
final_min_temp_stl_1988$time.series[,2]

# Turn the trend values into a dataframe to then ggplot
final_min_temp_trend_1988 <- as.data.frame(final_min_temp_stl_1988$time.series[,2]) %>% 
  mutate(dummy_day = seq.Date(from = as.Date("1988-01-01"),
                              by = 1,
                              length.out = 11684),
         x = as.numeric(x))


# Final plot of min temp trend from stl function
rr_daily_min_temp_1988 <- ggplot(final_min_temp_trend_1988,
                            aes(x = dummy_day,
                                y = x)) +
  geom_point(size = 1,
             colour = "gray50") +
  geom_smooth(method = "lm",
              colour = "black",
              linetype = "dashed",
              size = 2.5) +
  stat_cor(aes(label = paste(..rr.label.., "p<0.001", sep = "~`,`~")), # have to hard code the p value in
           label.y = 6,
           size = 7) +
  stat_regline_equation(label.y = 5,
                        size = 7) +
  coord_cartesian(ylim = c(0, 10.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 20,
                            face = "bold")) +
  scale_x_date(breaks = seq(as.Date("1988-01-01"), 
                            as.Date("2021-01-01"), 
                            by="5 years"),
               date_labels = "%Y") +
  labs(y = "Seasonally adjusted daily minimum temperature (°C)",
       x = "Date")


final_min_temp_trend_1988

min_temp_lm_1988 <- lm(x ~ dummy_day, data = final_min_temp_trend_1988)
summary(min_temp_lm_1988)
#
# Standard error of the daily min temp linear model, mutiplied by 39 years
summary.lm(min_temp_lm_1988)$coefficients[2,1] * 365 * 39
summary.lm(min_temp_lm_1988)$coefficients[2,2] *365 *39



#### __________ Growing degree days _________ ####


weather <- weather %>% 
       filter(max_temp > -20,
              year < 2020) %>% 
  mutate(ave_temp = (max_temp + min_temp)/2,
         gdd = if_else((ave_temp - 4) < 0,
                       0,
                       (ave_temp - 4)))

annual_gg <- weather %>% 
  group_by(year) %>% 
  summarise(year_gdd = sum(gdd))

ggplot(annual_gg,
       aes(x = year,
           y = year_gdd)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_regline_equation()


weekly_gdd <- weather %>% 
  mutate(my_week = round_date(day, "weeks")) %>% 
  group_by(my_week) %>% 
  summarise(my_weekly_gdd = sum(gdd))
  

# Weekly GDD for time series
gdd_time_series <-  weather %>% 
  mutate(my_week = round_date(day, "weeks")) %>% 
  group_by(my_week) %>% 
  summarise(my_weekly_gdd = sum(gdd))


weekly_gdd <- weekly_gdd %>% 
  mutate(my_month = month(my_week),
         week_of_year = week(my_week),
         my_year = year(my_week),
         decade = case_when(
           year(my_week) %in% c(seq(1980, 1989, 1)) ~ "eighties",
           year(my_week) %in% c(seq(1990, 1999, 1)) ~ "ninties",
           year(my_week) %in% c(seq(2000, 2009, 1)) ~ "naughties",
           year(my_week) %in% c(seq(2010, 2020, 1)) ~ "tens"),
         half_decade = case_when(
           year(my_week) %in% c(seq(1980, 1984, 1)) ~ "first",
           year(my_week) %in% c(seq(1985, 1989, 1)) ~ "second",
           year(my_week) %in% c(seq(1990, 1994, 1)) ~ "third",
           year(my_week) %in% c(seq(1995, 1999, 1)) ~ "fourth",
           year(my_week) %in% c(seq(2000, 2004, 1)) ~ "fifth",
           year(my_week) %in% c(seq(2005, 2009, 1)) ~ "sixth",
           year(my_week) %in% c(seq(2010, 2014, 1)) ~ "seventh",
           year(my_week) %in% c(seq(2015, 2019, 1)) ~ "eighth"),
         dummy_day = c(seq(1, length.out = 2088, by = 1))) %>% 
  filter(my_month %in% c(6, 7, 8))


ggplot(weekly_gdd,
       aes(x = my_week,
           y = my_weekly_gdd)) +
  geom_point() +
  geom_smooth()


ggplot(weekly_gdd,
       aes(x = dummy_day,
           y = my_weekly_gdd)) +
  coord_cartesian(ylim = c(0, 50)) +
  geom_smooth(method = "lm",
              colour = "black",
              linetype = "dashed",
              size = 2.5) +
  stat_cor(aes(label = paste(..rr.label.., "p<0.001", sep = "~`,`~")), # have to hard code the p value in
           label.y = 40,
           size = 7) +
  # stat_cor(label.y = 40,
  #          size = 7) +
  stat_regline_equation(label.y = 45,
                        size = 7) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 20,
                            face = "bold")) +
  # scale_x_date(breaks = seq(as.Date("1980-01-01"), 
  #                           as.Date("2021-01-01"), 
  #                           by="5 years"),
  #              date_labels = "%Y") +
  labs(y = "Growing degree days",
       x = "Date") +
  geom_point()



summary(lm(data = weekly_gdd, my_weekly_gdd ~ dummy_day))




winter_gdd <- weather %>% 
  filter(month %in% c(6, 7, 8)) %>% 
  group_by(year) %>% 
  summarise(my_winter_gdd = sum(gdd))

ggplot(winter_gdd,
       aes(x = year,
           y = my_winter_gdd)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_regline_equation()


summary(lm(data = winter_gdd, my_winter_gdd ~ year))


winter_gdd2 <- weather %>% 
  filter(month %in% c(6, 7, 8)) %>% 
  mutate(days_since_june_1 = day - as.Date(paste0(year(day), "-", "06-01")),
         decade = case_when(
           year(day) %in% c(seq(1980, 1989, 1)) ~ "eighties",
           year(day) %in% c(seq(1990, 1999, 1)) ~ "ninties",
           year(day) %in% c(seq(2000, 2009, 1)) ~ "naughties",
           year(day) %in% c(seq(2010, 2020, 1)) ~ "tens"))



ggplot(winter_gdd2,
       aes(x = days_since_june_1,
           y = gdd)) +
  geom_point() +
  geom_smooth(aes(colour = decade),
              method = "lm")

##      Time series analysis of weekly GDD
gdd_time_series <- gdd_time_series %>% 
  select(my_weekly_gdd)


gdd_ts <- ts(gdd_time_series, 
             start=decimal_date(ymd("1979-12-30")),
             frequency = 365.25/7)


# Use the stl function to generate the seasonal, trend and random error
final_gdd_stl <- stl(gdd_ts[, 1], s.window = "period")


# Plot of the four sections of it, data, seasonal, trend  and random error
plot(final_gdd_stl)


# Can extract the trend values by specifying the second column of the time.series data
final_gdd_stl$time.series[,2]

# Turn the trend values into a dataframe to then ggplot
final_gdd_trend <- as.data.frame(final_gdd_stl$time.series[,2]) %>% 
  mutate(dummy_day = seq.Date(from = as.Date("1979-12-30"),
                              by = 7,
                              length.out = 2088),
         x = as.numeric(x))


# Final plot of GDD trend from stl function
rr_gdd <- ggplot(final_gdd_trend,
                            aes(x = dummy_day,
                                y = x)) +
  geom_point(size = 1,
             colour = "gray50") +
  geom_smooth(method = "lm",
              colour = "black",
              linetype = "dashed",
              size = 2.5) +
  stat_cor(aes(label = 
                 paste(..rr.label.., 
                       ..p.label.., 
                       sep = "~`,`~")),# have to hard code the p value in
           label.y = 70,
           size = 8,
           family = "Times") +
  stat_regline_equation(label.y = 65,
                        size = 8,
                        family = "Times") +
  coord_cartesian(ylim = c(0, 75)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 22),
        axis.text = element_text(colour = "black",
                                 size = 22,
                                 family = "Times"),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_x_date(breaks = seq(as.Date("1980-01-01"), 
                            as.Date("2021-01-01"), 
                            by = "5 years"),
               date_labels = "%Y") +
  labs(y = "Seasonally-adjusted weekly growing degree days (°C)",
       x = "Date")

ggsave("RR gdd.png",
       plot = rr_gdd,
       dpi = 600,
       width = 12,
       height = 8)

annual_gdd <- weather %>% 
 group_by(year) %>% 
  summarise(yearly_total = sum(gdd, na.rm = T))
 
rr_gdd_annual <- ggplot(annual_gdd,
       aes(x = year,
             y = yearly_total)) +
  geom_point(size = 3,
             shape = 4) +
  geom_smooth(method = "lm",
              colour = "black",
              linetype = "dashed",
              size = 2) +
  stat_cor(aes(label = 
                 paste(..rr.label..,
                       "p<0.001", sep = "~`,`~")),
           label.y = 1000,
           size = 8, 
           family = "Times") +
  stat_regline_equation(label.y = 1500,
                        size = 8,
                        family = "Times") +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 22),
        axis.text = element_text(colour = "black",
                                 size = 22,
                                 family = "Times"),
        axis.ticks.length = unit(0.25, "cm")) +
  scale_x_continuous(breaks = seq(1980, 2020, by = 5)) +
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500)) +
  coord_cartesian(ylim = c(0, 3750)) +
  labs(x = "Year",
       y = "Annual growing degree days (°C)")

ggsave("RR annual gdd.png",
       plot = rr_gdd_annual,
       dpi = 600,
       width = 12,
       height = 8)


summary.lm(lm(yearly_total ~ year, annual_gdd))

## Winter GDD
  gdd_winter <- weather %>% 
    filter(month %in% c(6, 7, 8),
           day <= as.Date("2019-12-31")) %>% 
    select(gdd)
  
  # Convert the dataframe to a time series graph
  # Set frequency to 92 days
  winter_gdd_ts <- ts(gdd_winter, frequency = 92)
  
  
  # Use the stl function to generate the seasonal, trend and random error
  winter_gdd_stl <- stl(winter_gdd_ts[, 1], s.window = "period")
  
  
  # Plot of the four sections of it, data, seasonal, trend  and random error
  plot(winter_gdd_stl)
  
  
  # Can extract the trend values by specifying the second column of the time.series data
  winter_gdd_stl$time.series[,2]
  
  # Turn the trend values into a dataframe to then ggplot
  winter_gdd_trend <- as.data.frame(winter_gdd_stl$time.series[,2]) %>% 
    mutate(x = as.numeric(x)) %>% 
    rownames_to_column() %>% 
    rename(temp = x)
  
  winter_gdd_trend$rowname <- as.numeric(winter_gdd_trend$rowname)
  
  ## Plotting the trend
  rr_winter_gdd_plot <- ggplot(winter_gdd_trend,
                                          aes(x = rowname,
                                              y = temp)) +
    geom_point(size = 1,
               colour = "gray50") +
    geom_smooth(method = "lm",
                colour = "black",
                linetype = "dashed",
                size = 2.5) +
    stat_cor(aes(label = 
                   paste(..rr.label.., "p<0.001", sep = "~`,`~")),
             label.y = 2,
             size = 8,
             family = "Times") +
    stat_regline_equation(label.y = 3,
                          size = 8,
                          family = "Times") +
    coord_cartesian(ylim = c(0, 8)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(size = 1, linetype = "solid",
                                   colour = "black"),
          text = element_text(family = "Times",
                              size = 22),
          axis.text = element_text(colour = "black",
                                   size = 22,
                                   family = "Times"),
          axis.ticks.length = unit(0.25, "cm")) +
    scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ,10)) +
    scale_x_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500),
                       labels = comma) +
    labs(y = "Seasonally-adjusted winter growing degree days (°C)",
         x = "Winter days (June–August, inclusive) since 1 June 1980")
  
  
  ggsave("RR winter GDD.png",
         plot = rr_winter_gdd_plot,
         dpi = 600,
         width = 12,
         height = 8)

  # Extract coeffecients 
  
  gdd_lm <- lm(temp ~ rowname, data = winter_gdd_trend)
  
  
  summary.lm(gdd_lm)$coefficients[2,1] * 92 * 39
  summary.lm(min_temp_lm)$coefficients[2,2] * 92 *39
  
  
  
  #### Testing of coeffeictn of variations ####
  
  # https://cran.r-project.org/web/packages/cvequality/vignettes/how_to_test_CVs.html
  
  # Extract the rainfall data so I have the last 39 years, then allocate it to pre or post half
  #  of that time
  
  rain <- hawera_rainfall %>% 
    filter(date > as.Date("1979-12-31"),
           date < as.Date("2019-12-31")) %>% 
    group_by(my_month, my_year) %>% 
    summarise(month_rainfall = sum(rainfall))
    
  rain <- rain %>% 
    mutate(my_period = if_else(
      my_year < 2000,
      "pre_2000",
      "post_2000"
    )) %>% 
    group_by(my_month)

  ggplot(rain,
         aes(x = my_period,
             y = month_rainfall)) +
    geom_boxplot() +
    facet_wrap(.~my_month)
  
with(filter(rain,
            my_month == 9),
     asymptotic_test(month_rainfall,
                     my_period))






#### Cold stress index ####


# Formula = (11.7 + (3.1 x WS^0.5)) x (40 - T) + 481 + R
# 
# Where WS = average daily wind speed
# Where T = mean daily temperature ( max - min /2)
# Where R = 418 x (1-e^-0.04rain), where rain is the daily rainfall in millimetres

# Wind chill index
# 13.12 + 0.62 * average temperature - 13.17 * wind_run^0.16 + 0.40 * average temperature * wind_run^0.16


cold_stress <- weather %>% 
  filter(day > as.Date("1996-12-31")) %>% 
  mutate(CSI = (11.7 + (3.1 * wind_run^0.5)) * 
           (40 - ave_temp) + 481 + (418 * (1 - exp(-0.04 * rain))),
         WCI = (13.12 + (0.62 * ave_temp) - 
                  (13.17 * wind_run^0.16) + (.4 * ave_temp * wind_run^0.16)))


cold_stress %>% 
  group_by(year, month) %>% 
  summarise(my_CSI = mean(CSI)) %>% 
  ggplot(aes(x = year,
             y = my_CSI)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(.~month)


cold_stress %>% 
  filter(month %in% c(6, 7, 10, 11)) %>% 
  mutate(mating_period = case_when(
    month %in% c(6, 7) ~ "winter_mating",
    month %in% c(10, 11) ~ "spring-mating"
  )) %>% 
  ggplot(aes(x = mating_period,
             y = CSI)) +
  geom_boxplot() +
  geom_violin(alpha = 0.3,
              fill = "blue") +
  stat_compare_means(method = "kruskal.test")


cold_stress %>% 
  filter(month %in% c(6, 7, 10, 11)) %>% 
  mutate(mating_period = case_when(
    month %in% c(6, 7) ~ "winter_mating",
    month %in% c(10, 11) ~ "spring_mating"
  )) %>% 
  mutate(group_no = c(gl(n(), 3, n()))) %>% 
  group_by(mating_period,
           group_no) %>% 
  summarise(test = mean(CSI)) %>% 
  ggplot(aes(x = mating_period,
             y = test)) +
  geom_boxplot() +
  geom_violin(alpha = 0.3,
              fill = "blue") +
  stat_compare_means(method = "kruskal.test")


# Dot plot of CSI over time
cold_stress %>% 
  filter(day > as.Date("2017-06-01")) %>% 
  ggplot(aes(x = day,
           y = CSI)) +
  geom_point() +
  geom_hline(aes(yintercept = 1300))


cold_stress %>% 
  filter(day > as.Date("2017-06-01")) %>% 
  ggplot(aes(x = day,
             y = WCI)) +
  geom_point()
  #geom_hline(aes(yintercept = 1300))


# Number of days in a month above 1000 and 1300 CSI

cold_stress %>% 
  filter(day > as.Date("2017-06-01")) %>% 
  mutate(above_1000 = if_else(CSI >= 1000, T, F),
         above_1300 = if_else(CSI >= 1300, T, F)) %>% 
  group_by(year, month) %>% 
  summarise(over_1000 = sum(above_1000),
            over_1300 = sum(above_1300)) %>% 
  pivot_longer(over_1000:over_1300,
               names_to = "my_key",
               values_to = "my_value") %>% 
  ggplot(aes(x = as.factor(month),
             y = my_value)) +
  geom_col(aes(fill = as.factor(year)),
           position = "dodge") +
  facet_wrap(.~my_key)



#### Cliflo package ####


# Mkae my account
me <- cf_user(username = "jjarman",
              password = "hawera4672")

# Dataquery I want

my_data_query <- cf_datatype(4, 2, c(1, 4))

# The station agent number I want
my_station <- cf_station(25222)

hawera_cliflo <- cf_query(user = me,
         datatype = my_data_query,
         station = my_station,
         start_date = "2017-06-01-00",
         end_date = "2020-01-31-00")




# Practise for the rainfall plots

take_two_query <- cf_datatype(select_1 =     c(7,  4,  3,  2), 
            select_2 =     c(1,  2,  1,  1), 
            check_box = list(3,  1,  1,  4), 
            combo_box =    c(NA, NA, NA, 1))

hawera_cliflo_2 <- cf_query(user = me,
                          datatype = take_two_query,
                          station = my_station,
                          start_date = "2017-11-01-00",
                          end_date = "2020-01-31-00")

plot(hawera_cliflo_2,
     2, ggtheme = "light")


#### Climate averages during the mating period ####

mating_weather <- weather %>% 
  select(day,
         min_temp,
         max_temp,
         ave_temp,
         wind_run,
         rain) %>% 
  filter(day > as.Date("2018-06-06"))

AUT_1 <- interval(as.Date("2018-06-06"), (as.Date("2018-06-06") + days(77)) )
AUT_2 <- interval(as.Date("2019-06-05"), (as.Date("2019-06-05") + days(77)) )

SPR_1 <- interval(as.Date("2018-10-01"), (as.Date("2018-10-01") + days(77)))
SPR_2 <- interval(as.Date("2019-10-01"), (as.Date("2019-10-01") + days(77)))


mating_weather <- mating_weather %>% 
  mutate(treatment = 
           case_when(
             day %within% AUT_1 ~ "AUT",
             day %within% AUT_2 ~ "AUT",
             day %within% SPR_1 ~ "SPR",
             day %within% SPR_2 ~ "SPR"),
         mating = case_when(
           day %within% AUT_1 ~ 1,
           day %within% AUT_2 ~ 2,
           day %within% SPR_1 ~ 1,
           day %within% SPR_2 ~ 2)) %>% 
  filter(!is.na(mating)) %>% 
  pivot_longer(min_temp:rain,
               names_to = "my_key",
               values_to = "my_value")

ggplot(mating_weather,
       aes(x = treatment,
           y = my_value)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test",
                     label = "p.signif") +
  stat_summary(fun.y = median, colour = "darkred", geom = "point", size = 3, show.legend = FALSE) +
  stat_summary(fun.y = median, colour = "red", 
               geom = "text", show.legend = FALSE, 
               vjust = -0.7, aes(label = round(..y.., digits = 1))) +
  facet_wrap(my_key~mating,
             scales = "free")





              