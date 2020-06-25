## Script for the final cliflo data downloaded 3rd Feb 2020 ####

library(tidyverse)
library(scales)
library(lubridate)
library(ggpubr)
library(broom)
library(flextable)
library(extrafont)
#loadfonts(device = "win")
windowsFonts(Times = windowsFont("TT Times New Roman"))
library(ggfortify)
library(ggpmisc)
library(car)
library(modifiedmk)
library(changepoint)
library(lmerTest)
library(lmtest)
# change

hawera_rainfall <- read_csv("rainfall.csv",
                            col_names = T,
                            cols(date = col_date(format = "%d/%m/%Y")))

ohawe_rainfall <- read_csv("ohawe rainfall.csv",
                           col_names = T,
                           cols(date = col_date(format = "%d/%m/%Y")))

hawera_evapotrans <- read_csv("hawera evapotrans.csv",
                              col_names = T,
                              cols(date = col_date(format = "%d/%m/%Y")))

hawera_temp <- read_csv("hawera temperature.csv",
                        col_names = T,
                        cols(date = col_date(format = "%d/%m/%Y")))


# Mutating a month and year column

hawera_rainfall <- hawera_rainfall %>% 
  mutate(my_month = month(date),
         my_year = year(date))

ohawe_rainfall <- ohawe_rainfall %>% 
  mutate(my_month = month(date),
         my_year = year(date))


## Making a season column ####
# Making a season column that is based upon the month column ####

# Attempt 1 from https://stackoverflow.com/questions/49702002/creating-season-variable-by-month-with-dplyr-in-r
keyval <- data.frame(month = c("12", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"), 
                     season = rep(c("summer", "autumn", "winter", "spring"), each = 3),
                     stringsAsFactors = FALSE)

# Have to make the my_month column character after it has been made, so that it picks up the
#  small values (i.e. 1 and 2 were 10 and 20)

hawera_rainfall$my_month <- as.character(hawera_rainfall$my_month)

ohawe_rainfall$my_month <- as.character(ohawe_rainfall$my_month)

# Inner join the two to make a season column

hawera_rainfall <- inner_join(hawera_rainfall,
           keyval,
           by = c("my_month" = "month"))

ohawe_rainfall <- inner_join(ohawe_rainfall,
                             keyval,
                             by = c("my_month" = "month"))



# Making a period of time column

hawera_rainfall <- hawera_rainfall %>% 
  mutate(my_period = cut(date,
                         breaks = seq(ymd("1920-01-01"), ymd("2016-01-01"), '10 years'),
                         labels = F),
         my_month2 = month(date, label = T))

ohawe_rainfall <- ohawe_rainfall %>% 
  mutate(my_period = cut(date,
                         breaks = seq(ymd("1890-01-01"), ymd("2016-01-01"), '10 years'),
                         labels = F),
         my_month2 = month(date, label = T))

#### For Linda ####

# write_csv(hawera_rainfall,
#           "Hawera rainfall 1920-2020.csv")

# Calculate the monthly rainfall summaries and put into new dataframes

hawera_rainfall_sum <- hawera_rainfall %>% 
  group_by(stationID, my_period, season, my_year, my_month2) %>% 
  summarise(monthly_rainfall = sum(rainfall))

ohawe_rainfall_sum <- ohawe_rainfall %>% 
  group_by(stationID, my_period, season, my_year, my_month2) %>% 
  summarise(monthly_rainfall = sum(rainfall))


## Plotting the geom boxplots ####

hawera_rainfall_boxplot <- ggplot(hawera_rainfall_sum,
       aes(x = my_month2,
           y = monthly_rainfall)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y = mean,
               colour = "grey17",
               geom = "point", 
               size = 4,
               shape = 4,
               show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 250)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1,
                                 linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 11)) +
  scale_y_continuous(breaks = seq(0, 250, 25)) +
  labs(x = "",
       y = "Monthly rainfall (mm)")

ggsave("hawera rainfall boxplot.png",
       plot = hawera_rainfall_boxplot,
       dpi = 300,
       height = 6,
       width = 8)
  


hawera_rainfall %>% group_by(my_year) %>% 
  summarise(yearly_rainfall = sum(rainfall)) %>% 
  filter(my_year != 1920,
         my_year != 2019,
         yearly_rainfall > 500,
         yearly_rainfall < 2000) %>% 
  ggplot(aes(my_year,
             yearly_rainfall)) +
  geom_point() +
  coord_cartesian(ylim = c(0, 1600))

#### Materials and Methods ####
# Table of monthly/yearly rainfall for Materials and Methods

hawera_rainfall %>% group_by(my_year) %>% 
  summarise(yearly_rainfall = sum(rainfall)) %>% 
  filter(my_year != 1920,
         yearly_rainfall > 500,
         yearly_rainfall < 2000) %>% 
  summarise(average = mean(yearly_rainfall),
            my_2017 = yearly_rainfall[my_year == "2017"],
            my_2018 = yearly_rainfall[my_year == "2018"],
            my_2019 = yearly_rainfall[my_year == "2019"]) %>% 
  flextable()

# # Ohawe rainfall stats
# ohawe_rainfall %>% group_by(my_year) %>% 
#   summarise(yearly_rainfall = sum(rainfall)) %>% 
#   filter(yearly_rainfall > 500,
#          yearly_rainfall < 2000) %>% 
#   summarise(average = mean(yearly_rainfall),
#             my_2017 = yearly_rainfall[my_year == "2017"],
#             my_2018 = yearly_rainfall[my_year == "2018"],
#             my_2019 = yearly_rainfall[my_year == "2019"]) %>% 
#   flextable()


## Bar plot of the monthly rainfall summaries against the monthly average


# Is in VCN data at the moment


#### Farmers Forum - Plot for historic rainfall variation and rain days ####

# Rain days plots
hawera_rainfall <- hawera_rainfall %>% mutate(rain_day_1mm = if_else(rainfall >= 1,
                                              TRUE,
                                              FALSE,
                                              missing = NA),
                           rain_day_0.1mm = if_else(rainfall >= 0.1,
                                                  TRUE,
                                                  FALSE,
                                                  missing = NA))

rain_days <- hawera_rainfall %>% group_by(my_period,
                                          season,
                                          my_year,
                                          my_month2) %>% 
  summarise(num_rain_day_1mm = sum(rain_day_1mm),
            num_rain_day_0.1mm = sum(rain_day_0.1mm)) %>% 
  pivot_longer(num_rain_day_1mm:num_rain_day_0.1mm,
               names_to = "my_key",
               values_to = "my_value")



# Plot of the monthly rainday variation
rain_day_label <- c(num_rain_day_0.1mm = ">= 0.1mm rain days",
                    num_rain_day_1mm = ">= 1mm rain days")

ggplot(rain_days,
       aes(x = my_month2,
           y = my_value)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun.y = mean,
               colour = "grey17",
               geom = "text",
               aes(label = round(..y.., digits = 0)),
               vjust = -0.5,
               show.legend = FALSE) +
  facet_grid(my_key~.,
             labeller = labeller(my_key = rain_day_label)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 15)) + # Have to rewrite “Times” as the copy and paste doesn’t # make it go green
  labs(x = "",
       y = "Number of rain days per month")


# Plot of rain days over time for each period. Doesn't show much
ggplot(filter(rain_days,
              my_key == "num_rain_day_0.1mm"),
       aes(x = as.factor(my_period),
           y = my_value)) +
  geom_boxplot() +
  facet_wrap(my_month2~.)


#### -------- RESULTS - Rainfall during the experiment and also over time ####

hawera_rainfall

# Need a plot of rainfall variation per month to begin with
# Monthly rainfall based on twenty year periods?
# Making a new twenty year period column
hawera_rainfall <- hawera_rainfall %>% mutate(twenty_year_period =
                             cut(date,
                                 breaks = seq(ymd("1920-01-01"), ymd("2020-01-01"), '20 years'), 
                                 labels = c("period_1", "period_2", "period_3", "period_4", "period_5")))

hawera_monthly_rainfall <- hawera_rainfall %>% 
  group_by(season, twenty_year_period, my_year, my_month) %>% 
  summarise(monthly_rainfall = sum(rainfall))

# Plot of monthly rainfall over time per season
ggplot(hawera_monthly_rainfall,
       aes(x = my_year,
           y = monthly_rainfall)) +
  geom_point() +
  geom_smooth(method = lm,
              formula = y ~ x,
              se = 0.95) +
  stat_poly_eq(formula = y ~ x , aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE) +
  stat_cor(label.y = 250) +
  stat_regline_equation(label.y = 280) +
  facet_wrap(season~.)
# Shows that there is no statistical trend over the years





# Boxplot graph of the twenty year observation periods per season
my_period_comparisons1 <- list( c("period_1", "period_5"),
                               c("period_2", "period_5"),
                               c("period_3", "period_5"),
                               c("period_4", "period_5"),
                               c("period_1", "period_4"),
                               c("period_2", "period_4"),
                               c("period_3", "period_4"),
                               c("period_2", "period_3"),
                               c("period_1", "period_3"),
                               c("period_1", "period_2"))


ggplot(hawera_monthly_rainfall,
       aes(x = twenty_year_period,
           y = monthly_rainfall)) +
  geom_boxplot(outlier.shape = NA) +
  # scale_y_continuous(breaks = c(-15, -10, -5, 0, 5, 10, 15, 20),
  #                    name = expression("Seasonal average daily minimum air temperature "~(degree ~ C))) +
  # scale_x_discrete(name = "Observation period",
  #                  labels = c(period_1 = "1980 - 1987",
  #                             period_2 = "1988 - 1995",
  #                             period_3 = "1996 - 2003",
  #                             period_4 = "2004 - 2011",
  #                             period_5 = "2012 - 2019")) +
  stat_compare_means(comparisons = my_period_comparisons1,
                     label = "p.signif") +
  stat_summary(fun.y = mean,
               colour = "black",
               geom = "text", 
               size = 5,
               vjust = 5,
               aes(label = round(..y.., digits = 1)),
               show.legend = FALSE) +
  # coord_cartesian(ylim = c(-10, 55)) +
  facet_wrap(season~.,
             labeller = labeller(season = seasonal_labels)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 20))
# So no difference in monthly rainfall per season over each period at Hawera

## Boxplot of raindays per month per period per season
hawera_rain_days <- hawera_rainfall %>% group_by(season, twenty_year_period, my_year, my_month) %>% 
  summarise(rain_1mm = sum(rain_day_1mm),
            rain_0.1mm = sum(rain_day_0.1mm))

ggplot(filter(hawera_rain_days,
              !is.na(twenty_year_period)),
       aes(x = twenty_year_period,
           y = rain_0.1mm)) +
  geom_boxplot(outlier.shape = NA) +
  stat_compare_means(comparisons = my_period_comparisons1,
                     label = "p.signif") +
  stat_summary(fun.y = mean,
               colour = "black",
               geom = "text", 
               size = 5,
               vjust = 5,
               aes(label = round(..y.., digits = 1)),
               show.legend = FALSE) +
  facet_wrap(season~.,
             labeller = labeller(season = seasonal_labels)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 20))

## Scatter plot of the raindays over time with linear regression line

ggplot(hawera_rain_days,
       aes(x = my_year,
           y = rain_1mm)) +
  geom_point() +
  geom_smooth(method = lm,
              formula = y ~ x,
              se = 0.99) +
  stat_poly_eq(formula = y ~ x , 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  # stat_cor(label.y = 250) +
  # stat_regline_equation(label.y = 280) +
  facet_wrap(season~.)


## Rainfall over the experimental period ####
# Experiment timeline
hawera_rainfall_experiment <- hawera_rainfall %>% filter(date %within% interval("2017-06-01", "2020-01-31")) %>% 
  group_by(my_year, my_month) %>% 
  summarise(rainfall_monthly = sum(rainfall)) %>% 
  mutate(source = "experiment_time",
         my_day = "01",
         my_date = str_c(my_year, my_month, my_day, sep = "-"),
         dummy_date = ymd(my_date)) %>% 
  ungroup() %>% 
  select(rainfall_monthly, dummy_date, source, my_month)

# Average timeline
average_rainfall_2017 <- hawera_rainfall %>% filter(date %within% interval("1920-01-01", "2017-05-31")) %>% 
  group_by(my_year, my_month) %>% 
  summarise(rainfall_monthly = sum(rainfall)) %>% 
  group_by(my_month) %>% 
  summarise(rainfall_monthly = mean(rainfall_monthly)) %>% 
  mutate(my_day = "01",
         my_year = "2017",
         my_date = str_c(my_year, my_month, my_day, sep = "-"),
         dummy_date = ymd(my_date))

average_rainfall_2018 <- hawera_rainfall %>% filter(date %within% interval("1920-01-01", "2017-05-31")) %>% 
  group_by(my_year, my_month) %>% 
  summarise(rainfall_monthly = sum(rainfall)) %>% 
  group_by(my_month) %>% 
  summarise(rainfall_monthly = mean(rainfall_monthly)) %>% 
  mutate(my_day = "01",
         my_year = "2018",
         my_date = str_c(my_year, my_month, my_day, sep = "-"),
         dummy_date = ymd(my_date))

average_rainfall_2019 <- hawera_rainfall %>% filter(date %within% interval("1920-01-01", "2017-05-31")) %>% 
  group_by(my_year, my_month) %>% 
  summarise(rainfall_monthly = sum(rainfall)) %>% 
  group_by(my_month) %>% 
  summarise(rainfall_monthly = mean(rainfall_monthly)) %>% 
  mutate(my_day = "01",
         my_year = "2019",
         my_date = str_c(my_year, my_month, my_day, sep = "-"),
         dummy_date = ymd(my_date))

average_rainfall_2020 <- hawera_rainfall %>% filter(date %within% interval("1920-01-01", "2017-05-31")) %>% 
  group_by(my_year, my_month) %>% 
  summarise(rainfall_monthly = sum(rainfall)) %>% 
  group_by(my_month) %>% 
  summarise(rainfall_monthly = mean(rainfall_monthly)) %>% 
  mutate(my_day = "01",
         my_year = "2020",
         my_date = str_c(my_year, my_month, my_day, sep = "-"),
         dummy_date = ymd(my_date))


# Join the 2017, 2018, 2019, 2020 dataframes up, and then filter to remove the unwanted dates
experiment_binded <- bind_rows(average_rainfall_2017,
          average_rainfall_2018,
          average_rainfall_2019,
          average_rainfall_2020)

experiment_binded <- experiment_binded %>% 
  filter(dummy_date %within% interval("2017-06-01", "2020-01-01")) %>% 
  mutate(source = "long_term_average") %>% 
  select(rainfall_monthly, dummy_date, source, my_month)

# Join the experiment actuals with the long term averages
combined_experiment_long_term <- bind_rows(hawera_rainfall_experiment,
          experiment_binded)


experiment_rainfall_plot <- ggplot(combined_experiment_long_term,
       aes(x = dummy_date,
           y = rainfall_monthly)) +
  geom_col(aes(fill = source),
           position = "dodge",
           colour = "black",
           width = 15) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b-%Y",
               name = "",
               expand = expand_scale(mult = c(0.01, 0.01))) +
  scale_y_continuous(name = "Monthly rainfall (mm)",
                     limits = c(0, 210),
                     expand = expand_scale(0),
                     breaks = c(0, 25, 50, 75, 100, 125, 150, 175, 200)) +
  scale_fill_manual(values = c("black", 
                               "gray"),
                    name = "",
                    labels = c("Experimental period", 
                               "Long term average (1920-2016)")) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 22,
                            colour = "black"),
        axis.text = element_text(family = "Times",
                                 size = 22,
                                 colour = "black"),
        axis.text.x = element_text(angle = 90),
        axis.text.x.bottom = element_text(vjust = 0.5),
        axis.ticks.length = unit(0.25, "cm"),
        legend.position = "bottom")

  
ggsave("Experiment rainfall plot.png",
       plot = experiment_rainfall_plot,
       dpi = 600,
       width = 12,
       height = 10)


#### ________ Farmers forum deviation from average ________ ####


ff_rainfall_deviation <- combined_experiment_long_term %>% 
  pivot_wider(names_from = source,
              values_from = rainfall_monthly) %>% 
  mutate(my_deviation = (experiment_time - long_term_average)/long_term_average,
         above_or_below = if_else(my_deviation >= 0,
                                  T,
                                  F))


ff_rainfall_deviation_plot <- ggplot(ff_rainfall_deviation,
       aes(x = dummy_date,
           y = my_deviation)) +
  geom_col(fill = "black") +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(size = 28),
        legend.position = "bottom",
        axis.ticks.length = unit(.2, "cm")) +
  scale_y_continuous(name = "Difference from the long term average",
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(name = "",
               breaks = c(as.Date("2017-06-01"),
                               as.Date("2017-10-01"),
                               as.Date("2018-02-01"),
                               as.Date("2018-06-01"),
                               as.Date("2018-10-01"),
                               as.Date("2019-02-01"),
                               as.Date("2019-06-01"),
                               as.Date("2019-10-01"),
                               as.Date("2020-01-01")),
               date_labels = "%b-%Y")

ggsave("FF rainfall deviation plot.png",
       plot = ff_rainfall_deviation_plot,
       dpi = 300,
       width = 20,
       height = 8)




#### New M and M graph of rainfall for RESULTS ####

rr_deviation_rainfall <- ggplot(ff_rainfall_deviation,
       aes(x = dummy_date,
           y = my_deviation)) +
  geom_col(aes(fill = above_or_below),
           alpha = 0.7) +
  scale_y_continuous(name = "Difference from the long term average",
                     breaks = c(-1.0, -.75, -.5, -.25, 0, .25, .5, .75, 1),
                     labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(-1, 1)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b–%Y",
               name = "",
               expand = expand_scale(mult = c(0.01, 0.01))) +
    scale_fill_manual(values = c("red4", 
                               "green4"),
                    name = "") +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 22,
                            colour = "black"),
        axis.text = element_text(family = "Times",
                                 size = 22,
                                 colour = "black"),
        axis.text.x = element_text(angle = 90),
        axis.text.x.bottom = element_text(vjust = 0.5),
        axis.ticks.length = unit(0.25, "cm"),
        legend.position = "none")

ggsave("RR devaition rainfall.png",
       plot = rr_deviation_rainfall,
       dpi = 600,
       width = 12,
       height = 8)


# rain_experiment_results <- ggplot( +
#   geom_col(aes(x = test_date,
#                y = my_mean,
#                fill = source),
#            position = position_dodge(),
#            width = 20) +
#   scale_fill_manual(values = c("gray20",
#                                "black"),
#                     name = "Data",
#                     labels = c("Experimental period",
#                                "Long term average\n(1980 - 2020)")) +
#   scale_x_date(breaks = "2 months",
#                date_labels = "%b-%Y") +
#   scale_y_continuous(breaks = c(seq(0, 200, 25))) +
#   theme(panel.background = element_blank(),
#         axis.line = element_line(size = 1, linetype = "solid",
#                                  colour = "black"),
#         text = element_text(family = "Times",
#                             size = 30)) + # Have to rewrite “Times” as the copy and paste doesn’t # make it go green
#   labs(y = "mm",
#        x = "")
# 
# ggsave("FF Rainfall over the experimental period.png",
#        plot = rain_experiment,
#        dpi = 300,
#        width = 21,
#        height = 8)

#### ____________ Calculations for RESULTS __________ ####

# Have a look at the data
hawera_rainfall

ggplot(hawera_rainfall) +
  geom_point(aes(x = date,
                 y = rainfall))


ohawe_rainfall

ggplot(ohawe_rainfall) +
  geom_point(aes(x = date,
                 y = rainfall))


hawera_rainfall_sum <- hawera_rainfall_sum %>% 
  mutate(dummy_day = "01",
         joined_date = str_c(my_year, my_month2, dummy_day, sep = "-"),
         my_date = ymd(joined_date)) %>% 
  ungroup()

ggplot(hawera_rainfall_sum,
       aes(x = my_date,
           y = monthly_rainfall)) +
  geom_point() +
  geom_smooth(method = "lm") +
 # stat_regline_equation() +
  stat_cor() +
  facet_wrap(.~season)


ggplot(hawera_rainfall_sum,
       aes(x = season,
           y = monthly_rainfall)) +
  geom_boxplot()


# Plot of the number of raindays over time

hawera_rain_days <- hawera_rain_days %>% 
  ungroup() %>% 
  pivot_longer(rain_1mm:rain_0.1mm,
               names_to = "my_key_1",
               values_to = "my_value")

ggplot(hawera_rain_days,
       aes(x = my_year,
           y = my_value)) +
  geom_point(aes(colour = my_key_1)) +
  geom_smooth(aes(colour = my_key_1),
              method = "lm") +
  stat_regline_equation(label.y = 35) +
  stat_cor(label.y = 30) +
  facet_grid(my_key_1~season)



hawera_rainfall


#### ________ Days since rain calculations _______ ####

hawera_rainfall
# Remove all of the false rain values for 1mm
# Then do a lag of the dates to calculate the days since a rainfall event


hawera_days_since_rain <- hawera_rainfall %>% filter(rain_day_1mm == T) %>% 
  mutate(days_since_rain = as.numeric(date - lag(date))) %>% 
  filter(!is.na(days_since_rain),
         days_since_rain < 300) %>% 
  group_by(my_year, my_month) %>% 
  summarise(ave_days_since = mean(days_since_rain),
            median_days_since = median(days_since_rain),
            my_count = sum(!is.na(days_since_rain)))

# Plots of the average days since rain in each month in boxplot
ggplot(hawera_days_since_rain,
       aes(x = as.numeric(my_month),
           y = ave_days_since)) +
  geom_boxplot(aes(group = my_month))


# Boxplot of the number of observations per month
ggplot(hawera_days_since_rain,
       aes(x = as.numeric(my_month),
           y = my_count)) +
  geom_boxplot(aes(group = my_month))


# Geom_point of the number of days since rain over time

ggplot(hawera_days_since_rain,
       aes(x = my_year,
           y = ave_days_since)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(label.y = 20) +
  stat_regline_equation(label.y = 15) +
  facet_wrap(.~as.numeric(my_month))
# No significant difference in any month


# Plotting by season of year over the years

season_days_since_rain <- hawera_rainfall %>% filter(rain_day_1mm == T) %>% 
  mutate(days_since_rain = as.numeric(date - lag(date))) %>% 
  filter(!is.na(days_since_rain),
         days_since_rain < 300) %>% 
  group_by(my_year, season) %>% 
  summarise(ave_days_since = mean(days_since_rain),
            median_days_since = median(days_since_rain),
            my_count = sum(!is.na(days_since_rain)))


# Plot of seasonal days since rain, no apparent difference 
ggplot(filter(season_days_since_rain,
              ave_days_since < 15),
       aes(x = my_year,
           y = ave_days_since)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(label.y = 10) +
  stat_regline_equation(label.y = 7) +
  facet_wrap(.~season)


rr_seasonal_rainfall_summ_plot <- hawera_monthly_rainfall %>% 
  group_by(season, my_year) %>% 
  summarise(seasonal_rainfall = sum(monthly_rainfall)) %>% 
  ungroup() %>% 
  mutate(season = factor(season, levels = c("winter", "spring", "summer", "autumn"))) %>% 
  ggplot(aes(x = my_year,
             y = seasonal_rainfall)) +
  geom_point(shape = 4) +
  geom_smooth(method = "lm",
              colour = "black",
              linetype = "dashed") +
  stat_regline_equation(label.y = 50,
                        size = 6) +
  stat_cor(label.y = 5,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           size = 6) +
  facet_wrap(.~season, labeller = labeller(season = seasonal_labels)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",
                                 colour = "black"),
        text = element_text(family = "Times",
                            size = 20,
                            face = "bold")) +
  scale_x_continuous(breaks = seq(from = 1920, to = 2020, by = 20),
                     name = "Year") +
  labs(y = "Accumulated rainfall per season (mm)")


ggsave("RR seasonal rainfall.png",
       plot = rr_seasonal_rainfall_summ_plot,
       dpi = 600,
       width = 12,
       height = 8)




#### ______ Durbin Watson testing of rainfall _________ ####


## Make a linear regression of seasonal rainfall
# Test dataframe of the seasonal rainfall in each year
test <- hawera_rainfall %>% 
  group_by(my_year,
           season) %>% 
  summarise(seasonal_rain = sum(rainfall, na.rm = T)) %>% 
  group_by(season)

# Linear regressions for each season over time
do(test, tidy(lm(.$seasonal_rain ~ .$my_year)))


# Residual plot of the linear model for spring
  plot(resid(lm(data = filter(test, 
                   season == "spring"),
     seasonal_rain ~ my_year)))

  
  

  ## Make a time series of the daily rainfall
  
  hawera_rain_ts <- hawera_rainfall %>% 
    filter(date > as.Date("1920-12-31")) %>% 
    select(rainfall) %>% 
    ts(start = c(1920, 1), frequency = 365.25)
  
    
  autoplot(stl(hawera_rain_ts[, 1],
      s.window = "period"))
  
autoplot(acf(hawera_rain_ts, plot = F))
  
# DW test for the daily rainfall data
dw <- durbinWatsonTest(lm(data = filter(hawera_rainfall,
          season == "autumn"),
   rainfall ~ date))

tidy(dw)


plot(lm(data = filter(hawera_rainfall,
                      season == "autumn"),
        rainfall ~ date))



#### Second go at Durbin Watson testing of autocorrelation ####

plot(decompose(hawera_rain_ts,
               type = "multiplicative"))

# Using the modifiedmk package
# mmkh(hawera_rain_ts$rainfall)
# hawera_rain_ts$rainfall
# 
# 
# 
# guess <- data.frame(hawera_rain_ts)
# 
# summary(mmkh(guess$rainfall))



# Monthly rainfall decomposed
hawera_rainfall_monthly <- hawera_rainfall %>% 
  filter(date > as.Date("1977-04-01")) %>% 
  group_by(my_year, my_month) %>% 
  summarise(monthly_rainfall = sum(rainfall)) %>% 
  ungroup() %>% 
  mutate(my_dummy = paste0(my_year, "/", my_month, "/", "01"),
         my_date = ymd(my_dummy))

# plot of monthly rainfall
ggplot(hawera_rainfall_monthly,
       aes(x = my_date,
           y = monthly_rainfall)) +
  geom_point() +
  geom_smooth(method = "lm")


# Time series of Hawera monthly rainfall
hawera_ts_monthly <- hawera_rainfall %>% 
  filter(date > as.Date("1977-04-01")) %>% 
  group_by(my_year, my_month2) %>% 
  summarise(monthly_rainfall = sum(rainfall)) %>% 
  ungroup() %>% 
  select(monthly_rainfall) %>% 
  ts(start = c(1977, 4), end = c(2020, 1), frequency = 12)


# Seasonal decompositon of monthly rainfall
plot(decompose(hawera_ts_monthly))


# Changepoints plotting
mvalue <- cpt.mean(hawera_ts_monthly[,1],
                   method = "BinSeg",
                   Q = 40)

cpts(mvalue)

plot(mvalue)

vvalue <- cpt.var(diff(hawera_ts_monthly[,1]), method = "PELT")
cpts(vvalue)
plot(vvalue)


# Mann kendall test with the Yue and Wang approach # Works
mmky(c(hawera_ts_monthly[,1]))


# Durbin Watson test for the monthly rainfall
durbinWatsonTest(lm(data = hawera_rainfall_monthly,
                    monthly_rainfall ~ my_date))




#### Monthly Goldfeld-Quant testing ####

gq_monthly <- hawera_rainfall  %>% 
  #filter(date > as.Date("1977-04-01")) %>% 
  group_by(my_year, my_month) %>% 
  summarise(monthly_rainfall = sum(rainfall)) %>% 
  ungroup() %>% 
  mutate(my_dummy = paste0(my_year, "/", my_month, "/", "01"),
         my_date = ymd(my_dummy)) %>% 
  arrange(my_date)


# Goldfeld-quant test
# Lineary model
lm_gq_monthly <- lm(data = gq_monthly,
            monthly_rainfall ~ my_date)


gqtest(lm_gq_monthly, order.by = ~my_date,
       data = gq_monthly,
       fraction = 0.2)

#### Yearly Goldfeld-Quant testing ####

gq_yearly <- hawera_rainfall  %>% 
  filter(date > as.Date("1920-12-31")) %>% 
  group_by(my_year) %>% 
  summarise(yearly_rainfall = sum(rainfall)) %>% 
  ungroup() %>% 
  arrange(my_year)

# Goldfeld-quant test
# Lineary model
lm_gq_yearly <- lm(data = gq_yearly,
                    yearly_rainfall ~ my_year)


gqtest(lm_gq_yearly, order.by = ~my_year,
       data = gq_yearly,
       fraction = 0.1)
# Trending to significance

#### Breaush-Pagan test of the monthly rainfall ####

bptest(lm_gq_monthly)
# Not significant

#### Breaush-Pagan test of the yearly rainfall ####

bptest(lm_gq_yearly)
# Not significant



#### Number of rain days ####

rain_days <- hawera_rainfall %>% 
  filter(date > as.Date("1920-12-31")) %>%
  group_by(my_month,
           my_year) %>% 
  summarise(rain_days = sum(rain_day_1mm)) %>% 
  ungroup() %>% 
  mutate(my_dummy = paste0(my_year, "/", my_month, "/", "01"),
         my_date = ymd(my_dummy)) %>% 
  arrange(my_date)

ggplot(rain_days,
       aes(x = my_year,
           y = rain_days)) +
  geom_point() +
  geom_smooth(method = "lm")



rain_day_lm <- lm(data = rain_days,
                  rain_days ~ my_date)

# Goldfeld-Quant test of rain days
gqtest(rain_day_lm,
       data = rain_days,
       order.by = ~my_date,
       fraction = 0.1)

# Breaush-Pagan test of raindays
bptest(rain_day_lm)



citation("lmtest")





  