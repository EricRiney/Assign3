# Ashlyn and Eric 
# INFO 370

library(MASS) # need fractions

setwd("/Users/ericriner/Documents/Code/UW/370/Assign3")

weather = read.csv(file.choose(), header = TRUE)
crime = read.csv(file.choose(), header = TRUE)

summary(weather)
summary(crime)

#GOAL: FIND OUT IF THERE IS A CORRELATION BETWEEN WEATHER AND CRIME RATES

hist(weather$Temp.Avg)
(data.frame(table(crime$Event.Clearance.Date)))
table(format(crime$Event.Clearance.Date, "%b-%Y"))

install.packages("lubridate")
library(lubridate)

crimeDate = mdy_hms(crime$Event.Clearance.Date)
crime$Months = month(crimeDate)

boxplot(Temp.Avg ~ Month, data=weather)

View(crimeDate$months)

(data.frame(table(crimeMonths)))

crime$Months = factor(crime$Months)
contrasts(crime$Months) = "contr.sum"

t.test(sum(complete.cases(crime)),crime$Months)

totalCrimes = sum(complete.cases(crime$Event.Clearance.Group))

