# Welcome to R

# get working directory
getwd()
# set working directory to that folder
setwd("C:\\Users\\Admin\\Desktop\\R code")
# get working directory again - to confirm
getwd()

# Hello World
print("Hello R world!")

# Data Types
i <- 1
class(i)  
typeof(i) 

x <- 3.14
class(x)  
typeof(x) 

company <- "QA Ltd"
class(company)  
typeof(company) 

passed <- TRUE
class(passed)
typeof(passed)

is.integer(i)
# everything numeric is by default double

# coerce into integer
j <- as.integer(i)   
is.integer(j) 

length(i)     
length(company)

# Vectors
is.vector(i)
is.vector(x)
is.vector(company)
is.vector(passed) 
# by default everything is a vector with one element

colour <- c("red", "white", "blue") 
colour[2]

values <- 1:5   
values[2]
values > 3
values[values > 3]
values[values > 4 | values < 3]
values[values >= 2 & values <= 4]

# Data Frame

# read a set of data from file
data <- read.csv("gapminder.csv")
#
data

# data type
is.data.frame(data)

#
# Preview data
#
data
head(data, 3)
tail(data, 2)

#
# Data profiling
#
dim(data)
nrow(data)
ncol(data)
colnames(data)
str(data)
summary(data)

# access single column from a dataset (using dollar symbol)
data$population
# save into a new variable (and observe it appearing under the Environment window)
pop <- data$population
le  <- data$life_expectancy
# and output it:
pop

length(data$region)
is.vector(data$region)
is.factor(data$region)

# select columns
data$population
data2 <- data[,c("population", "age5_surviving")]

# remove columns
data7 <- within(data, rm("gdp_per_capita","gdp_per_day"))

# filter rows
data[data$year == 2015,]

# create new variable called y2013 which contains data for 
# all the countries for years 2013 onwards
y2013 <- data[data$year >= 2013, ]
# how can we prove that the dataset above only contains 
# records that refer to years 2013 onwards?
unique(y2013$year)

# get all the data for Europe where life expectancy is over 82
europe_82a <- data[data$life_expectancy > 82 & data$region == "Europe",]
europe_82a
# check that is is for Europe only
unique(europe_82a$region)
# check that min life_expectancy is over 82
min(europe_82a$life_expectancy)

# list of countries from the subset europe_82a
unique(europe_82a$country)

# get data(country, year, gdp_per_capita) for Europe only with life expectancy over 82
europe_82 <- data[data$region == "Europe" & data$life_expectancy > 82,
  c("country", "year", "gdp_per_capita")]
dim(europe_82)
view(europe_82)

# get (country, gdp_per_capita) for Asia in 2015
GDP_Asia_2015 <- data[data$region == "Asia" & data$year == 2015, 
                      c("country", "gdp_per_capita")]
View(GDP_Asia_2015)

# add new column
data$gdp_new <- data$gdp_per_capita/365
dim(data)
data$pop_mill <- data$population/1000000
dim(data)
View(data)

# remove column 
data$gdp_new <- NULL
data$pop_mill <- NULL
dim(data)
View(data)

#
# changing data types (casting)
#
str(data)
data$region  <- as.factor(data$region)
data$country <- as.factor(data$country)
data$year    <- as.integer(data$year)
str(data)
