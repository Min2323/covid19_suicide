library(CausalImpact)
library(zoo)
library(dplyr)

## suicide ## 
varData <- aggregate(suicide~yearMonth,data=nedis,FUN = sum)
varData <- varData[,-1]


timePoints<- seq.Date(as.Date("2016-01-01"),as.Date("2020-12-01"),by = "months")
data <- zoo(cbind(varData),timePoints)

pre.period <- as.Date(c("2016-01-01","2020-02-01"))
post.period <- as.Date(c("2020-03-01","2020-12-01"))

impact <- CausalImpact(data,pre.period,post.period)
plot(impact)


## selfharm ## 
varData <- aggregate(selfHarm~yearMonth,data=nedis,FUN = sum)
varData <- varData[,-1]


timePoints<- seq.Date(as.Date("2016-01-01"),as.Date("2020-12-01"),by = "months")
data <- zoo(cbind(varData),timePoints)

pre.period <- as.Date(c("2016-01-01","2020-02-01"))
post.period <- as.Date(c("2020-03-01","2020-12-01"))

impact <- CausalImpact(data,pre.period,post.period)
plot(impact)

## suicide + selfharm ##  
varData <- aggregate(selfHarm+suicide~yearMonth,data=nedis,FUN = sum)
varData <- rename(varData,observed =`selfHarm + suicide` )
varData <- varData[,-1]


timePoints<- seq.Date(as.Date("2016-01-01"),as.Date("2020-12-01"),by = "months")
data <- zoo(cbind(varData),timePoints)

pre.period <- as.Date(c("2016-01-01","2020-02-01"))
post.period <- as.Date(c("2020-03-01","2020-12-01"))

impact <- CausalImpact(data,pre.period,post.period)
plot(impact)



## young female suicide ## 
youngFemale <- female %>%
  filter(ageFactor == 2)


varData <- aggregate(suicide~yearMonth,data=youngFemale,FUN = sum)
varData <- varData[,-1]


timePoints<- seq.Date(as.Date("2016-01-01"),as.Date("2020-12-01"),by = "months")
data <- zoo(cbind(varData),timePoints)

pre.period <- as.Date(c("2016-01-01","2020-02-01"))
post.period <- as.Date(c("2020-03-01","2020-12-01"))

impact <- CausalImpact(data,pre.period,post.period)
plot(impact)


## young female selfharm ## 
varData <- aggregate(selfHarm~yearMonth,data=youngFemale,FUN = sum)
varData <- varData[,-1]


timePoints<- seq.Date(as.Date("2016-01-01"),as.Date("2020-12-01"),by = "months")
data <- zoo(cbind(varData),timePoints)

pre.period <- as.Date(c("2016-01-01","2020-02-01"))
post.period <- as.Date(c("2020-03-01","2020-12-01"))

impact <- CausalImpact(data,pre.period,post.period)
plot(impact)

## young female suicide + selfharm ## 
varData <- aggregate(selfHarm+suicide~yearMonth,data=youngFemale,FUN = sum)
varData <- rename(varData,observed =`selfHarm + suicide` )
varData <- varData[,-1]


timePoints<- seq.Date(as.Date("2016-01-01"),as.Date("2020-12-01"),by = "months")
data <- zoo(cbind(varData),timePoints)

pre.period <- as.Date(c("2016-01-01","2020-02-01"))
post.period <- as.Date(c("2020-03-01","2020-12-01"))

impact <- CausalImpact(data,pre.period,post.period)
plot(impact)
