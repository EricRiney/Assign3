# Eric 
# INFO 370

library(MASS) # need fractions
require(zoo)  # date stuff
library(lubridate)
require(ggplot2)


setwd("/Users/ericriner/Documents/Code/UW/370/Assign3")

weather = read.csv(file.choose(), header = TRUE)
crime = read.csv(file.choose(), header = TRUE)


burglaryCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
burglaryCrime2012Results = vector(mode="numeric", length=length(burglaryCrime2012Vec))
for (i in 1:length(burglaryCrime2012Vec)) {
  counter = 0
  #for (j in 1:length(burglaryCrime2012Vec)) {
  if(crime$Summarized.Offense.Description[i] == 'BURGLARY'){
    counter = counter + 1
    burglaryCrime2012Results[i] = counter
    # }
  }
}
plot(weather2012DatesResults,burglaryCrime2012Results)
plot

boxplot(summary(weather2012DatesResults),summary(burglaryCrime2012Results),
        main="Overcast vs Burglary",
        notch = TRUE, add = TRUE, col = "grey")
#plot(weather2012DatesResults)

summary(burglaryCrime2012Results)
summary(weather2012DatesResults)

