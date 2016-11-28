# Eric 
# INFO 370

library(MASS) # need fractions
require(zoo)  # date stuff
library(lubridate)

setwd("/Users/ericriner/Documents/Code/UW/370/Assign3")

weather = read.csv(file.choose(), header = TRUE)
crime = read.csv(file.choose(), header = TRUE)

summary(weather)
summary(crime)