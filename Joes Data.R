#### Import the data

library(tidyverse)
library("correlation")
library(broom)
library(ggpubr)
library(GGally)



joe <- read_csv("joes data.csv")

# Correlations 
joe_corr <- correlation(joe)
# Gives the p values etc.

ggpairs(joe)

# Linear models
# Change data around
joe1 <- joe %>% 
  pivot_longer(MS:rainfall,
               names_to = "my_key",
               values_to = "my_value")

ggplot(joe1,
       aes(x = year,
           y = my_value)) +
  geom_line() +
  geom_smooth(method = "lm",
              colour = "blue") +
  stat_regline_equation(label.x.npc = "left") +
  stat_cor(aes(label = 
                      paste(..rr.label.., 
                            ..p.label.., 
                            sep = "~`,`~")),
           label.x.npc = "centre") +
  facet_grid(my_key~.,
             scales = "free_y")

## Month data

joe_month <- read_csv("joes month data.csv")

joe_month <- joe_month %>% 
  pivot_longer(July:June,
               names_to = "my_key",
               values_to = "my_value")

# Change factor levels of months
joe_month$my_key <- factor(joe_month$my_key,
          levels = c("June",
                     "July",
                     "August",
                     "September",
                     "October",
                     "November",
                     "December",
                     "January",
                     "February",
                     "March",
                     "April",
                     "May"))

joes_month_plot <- ggplot(joe_month,
       aes(x = my_year,
           y = my_value)) +
  geom_point() +
  geom_smooth(method = "lm",
              colour = "blue") +
  stat_regline_equation(label.x.npc = "left") +
  stat_cor(aes(label = 
                 paste(..rr.label.., 
                       ..p.label.., 
                       sep = "~`,`~")),
           label.x.npc = "centre") +
  facet_wrap(.~my_key) +
  labs(title = "Monthly pasture growth rate (kg DM/ha/day) at Waimate West between 2001 and 2018",
       y = "Pasture growth rate (kg DM/ha/day)",
       x = "Year")

ggsave("Joes month plot.png",
       dpi = 300,
       width = 15,
       height = 10)





#### R Community ####

# Structure of the data
head(joe)

# Can make a linear model of milksolids production
lm_1 <- lm(MS ~ year, data = joe)

summary(lm_1)

# Or a linear model of pasture growth each year
lm_2 <- lm(pasture_grown ~ year, data = joe)

summary(lm_2)

# Or a linear model for rainfall each year
lm_3 <- lm(rainfall ~ year, data = joe)

summary(lm_3)

## Can plot this data

# Have to first make the data tidy
joe_long <- joe %>% 
  pivot_longer(MS:rainfall,
               names_to = "my_key",
               values_to = "my_value")

# Plot
ggplot(joe_long,
       aes(x = year,
           y = my_value)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm",
              colour = "blue",
              se = F) +
  stat_regline_equation(label.x.npc = "left") +
  stat_cor(aes(label = 
                 paste(..rr.label.., 
                       ..p.label.., 
                       sep = "~`,`~")),
           label.x.npc = "centre") +
  facet_grid(my_key~.,
             scales = "free_y")


# What if we want to look at interactions?

# MS with an interaction of pasture grown
lm_4 <- lm(MS ~ year + pasture_grown, data = joe)

# Can do a summary, but is not a nice format to work with
summary(lm_4)

# So can use the tidy function from the broom package
tidy(summary(lm_4))




# Using broom to compute multiple models in one go

joe %>% 
  nest(-year) %>% 
  mutate(
    linear_models = map(data, ~lm(MS ~ rainfall, data = .x)),
    tidied = map(linear_models, tidy)
  ) %>% 
  unnest(tidied)
  











