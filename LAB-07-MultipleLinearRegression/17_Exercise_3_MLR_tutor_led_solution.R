#
# Multiple Linear Regression
#
setwd("C:\\Users\\Admin\\Desktop\\R code")
install.packages("tidyverse")
library(tidyverse)

# read the data
data <- read_csv("gapminder.csv")

# filter only the rows for Asia
# remove the region column
Asia <- data %>% filter(region=="Asia") %>% select(-region)
# OR
Asia <- subset(data,region=="Asia",select = -c(region))

#
# Build MLR
#
mlr <-lm(life_expectancy ~ age5_surviving + babies_per_woman + population + gdp_per_day, Asia)
summary(mlr)

# We notice that variable population has high p-value (0.0866),
# therefore it has no (or very little) predicting power,
# and is statistically insignificant
# We will exclude it from the model

mlr <-lm(life_expectancy ~ age5_surviving + babies_per_woman + gdp_per_day, Asia)
summary(mlr)

# R-squared value is 0.9245, which means that population had miserable effect on the model,
# more than 92% of data is still explained by it
# In real life, anything above 60% is considered to be good

# Display coefficients
# They can also be seen in the first column of summary(mlr) command
mlr$coefficients

# The first value is intercept, the rest are coefficients for each variable -
# but in a multi dimensional space,
# so would be very difficult to visualise

# formula:
life_expectancy = 0.90867805*age5_surviving +
  -1.04411231*babies_per_woman +
  0.02591394*gdp_per_day + (-16.02050802)
