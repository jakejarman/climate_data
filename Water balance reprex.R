library(tidyverse)
# Make dataframe
df <- tribble(
  ~a, ~b,
  3, 1,
  4, 2,
  3, 3,
  4, 4,
  3, 5,)
df
# Specify k
k <- 10
# mutate a new column that is the lagged value of d plus a minus b
df %>% mutate(d = case_when(
  row_number() == 1 ~ k + a - b,
  row_number() != 1 ~ lag(z) + a - b))
#actual_df
tribble(
  ~a, ~b, ~c,
  3, 1, 12,
  4, 2, 14,
  3, 3, 14,
  4, 4, 14,
  3, 5, 12)



# https://stackoverflow.com/questions/37994136/r-is-it-possible-to-use-mutatelag-with-the-same-column


set.seed(2)
t = data.frame(week = seq(1:52),
               z = runif(52, 0, 100),
               b = 0.2)


for (row in 2:dim(t)[1]) {
  t[row,] <- mutate(t[1:row,], x= lag(x,1) * b + z * (1 - b))[row,]
}



# https://stackoverflow.com/questions/46668095/how-to-create-a-column-which-use-its-own-lag-value-using-dplyr

library(tidyverse)
k <- 10 # Set a k value
df1 <- tribble(
  ~a, ~b,
  1,  1,
  1,  2,
  1,  3,
  1,  4,
  1,  5,)
# Base approach
df1$c <- df1$a - df1$b
df1[1, "c"] <- k
df1$c <- cumsum(df1$c)
df1
# New df
df2 <- tribble(
  ~a, ~b,
  1,  1,
  1,  2,
  1,  3,
  1,  4,
  1,  5,)
# dplyr approach
df2 %>% 
  mutate(c = lag(cumsum(a - b), 
                 default = k))
# Gives two different dataframes

library(tidyverse)
# Desired output
tribble(
  ~a, ~b, ~c,
  1, 1, 10,
  1, 2, 9,
  1, 3, 7,
  1, 4, 4,
  1, 5, 0)
df2 <- tribble(
  ~a, ~b,
  1,  1,
  1,  2,
  1,  3,
  1,  4,
  1,  5,)
k <- 10
df2 %>% 
  mutate(c = case_when(
    row_number() == 1 ~ k,
    row_number() != 1 ~ lag(c) + a - b))


df2 %>% mutate(c = c(k, k + cumsum(a - b)[-1]))








