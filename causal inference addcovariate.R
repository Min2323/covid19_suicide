
library(CausalImpact)
library(zoo)
library(dplyr)

nedis$femaleSuicide <- ifelse((nedis$PTMISEXX == "F") & (nedis$suicide == 1),1,0)
nedis$femaleSelfHarm<- ifelse((nedis$PTMISEXX == "F") & (nedis$selfHarm == 1),1,0)
nedis$female <- ifelse(nedis$PTMISEXX == "F",1,0)

## 손상기전 ##
for (i in 1:8) {
  nedis <- nedis %>%
    mutate(!!paste0("arcs",i) := ifelse(ARCS == i,1,0))
}


## 보험 ## 
num <- c(as.numeric(levels(nedis$PTMIIUKD)))

for (i in num) {
  nedis <- nedis %>%
    mutate(!!paste0("iukd",i) := ifelse(PTMIIUKD == i,1,0))
}

#### causal inference ####
## suicide ## 
nedisSuicide <- nedis %>%
  filter(suicide == 1)


meanAge <- aggregate(PTMIBRTD ~ yearMonth,data= nedisSuicide, FUN = mean)
varData <- aggregate(cbind(suicide,femaleSuicide,arcs1,arcs2,arcs3,arcs4,arcs5,arcs6,arcs7,arcs8,
                           iukd10,iukd20,iukd30,iukd51,iukd52,iukd60,iukd88,iukd99) ~ yearMonth,data= nedisSuicide, FUN = sum)
varData <- merge(varData, meanAge, by = "yearMonth")
varData <- varData[,-1]


timePoints<- seq.Date(as.Date("2016-01-01"),as.Date("2020-12-01"),by = "months")
data <- zoo(cbind(varData),timePoints)

pre.period <- as.Date(c("2016-01-01","2020-02-01"))
post.period <- as.Date(c("2020-03-01","2020-12-01"))

impact <- CausalImpact(data,pre.period,post.period)
plot(impact)


## selfharm ##
nedisSelfHarm <- nedis %>%
  filter(selfHarm == 1)

meanAge <- aggregate(PTMIBRTD ~ yearMonth,data= nedisSelfHarm, FUN = mean)
varData <- aggregate(cbind(selfHarm,femaleSelfHarm,arcs1,arcs2,arcs3,arcs4,arcs5,arcs6,arcs7,arcs8,
                           iukd10,iukd20,iukd30,iukd51,iukd52,iukd60,iukd88,iukd99) ~ yearMonth,data= nedisSelfHarm, FUN = sum)
varData <- merge(varData, meanAge, by = "yearMonth")
varData <- varData[,-1]


timePoints<- seq.Date(as.Date("2016-01-01"),as.Date("2020-12-01"),by = "months")
data <- zoo(cbind(varData),timePoints)

pre.period <- as.Date(c("2016-01-01","2020-02-01"))
post.period <- as.Date(c("2020-03-01","2020-12-01"))

impact <- CausalImpact(data,pre.period,post.period)
plot(impact)


## suicide + selfharm ##
meanAge <- aggregate(PTMIBRTD ~ yearMonth,data= nedis, FUN = mean)
varData <- aggregate(cbind(selfHarm+suicide,female,arcs1,arcs2,arcs3,arcs4,arcs5,arcs6,arcs7,arcs8,
                           iukd10,iukd20,iukd30,iukd51,iukd52,iukd60,iukd88,iukd99) ~ yearMonth,data= nedis, FUN = sum)
varData <- rename(varData,observed = V1 )
varData <- merge(varData, meanAge, by = "yearMonth")
varData <- varData[,-1]


timePoints<- seq.Date(as.Date("2016-01-01"),as.Date("2020-12-01"),by = "months")
data <- zoo(cbind(varData),timePoints)

pre.period <- as.Date(c("2016-01-01","2020-02-01"))
post.period <- as.Date(c("2020-03-01","2020-12-01"))

impact <- CausalImpact(data,pre.period,post.period)
plot(impact)

## young female suicide ## 

youngfemaleSui <- nedis %>%
  filter(PTMISEXX == "F") %>%
  filter(ageFactor == 2) %>%
  filter(suicide == 1)
  

meanAge <- aggregate(PTMIBRTD ~ yearMonth,data= youngfemaleSui, FUN = mean)
varData <- aggregate(cbind(suicide,arcs1,arcs2,arcs3,arcs4,arcs5,arcs6,arcs7,arcs8,
                           iukd10,iukd20,iukd30,iukd51,iukd52,iukd60,iukd88,iukd99) ~ yearMonth,data= youngfemaleSui, FUN = sum)
varData <- merge(varData, meanAge, by = "yearMonth")
varData <- varData[,-1]


timePoints<- seq.Date(as.Date("2016-01-01"),as.Date("2020-12-01"),by = "months")
data <- zoo(cbind(varData),timePoints)

pre.period <- as.Date(c("2016-01-01","2020-02-01"))
post.period <- as.Date(c("2020-03-01","2020-12-01"))

impact <- CausalImpact(data,pre.period,post.period)
plot(impact)

## young female selfharm ## 

youngfemaleSelf <- nedis %>%
  filter(PTMISEXX == "F") %>%
  filter(ageFactor == 2) %>%
  filter(selfHarm == 1)


meanAge <- aggregate(PTMIBRTD ~ yearMonth,data= youngfemaleSelf, FUN = mean)
varData <- aggregate(cbind(selfHarm,arcs1,arcs2,arcs3,arcs4,arcs5,arcs6,arcs7,arcs8,
                           iukd10,iukd20,iukd30,iukd51,iukd52,iukd60,iukd88,iukd99) ~ yearMonth,data= youngfemaleSelf, FUN = sum)
varData <- merge(varData, meanAge, by = "yearMonth")
varData <- varData[,-1]


timePoints<- seq.Date(as.Date("2016-01-01"),as.Date("2020-12-01"),by = "months")
data <- zoo(cbind(varData),timePoints)

pre.period <- as.Date(c("2016-01-01","2020-02-01"))
post.period <- as.Date(c("2020-03-01","2020-12-01"))

impact <- CausalImpact(data,pre.period,post.period)
plot(impact)

## young female selfharm + suicide ## 

youngFemale <- nedis %>%
  filter(PTMISEXX == "F") %>%
  filter(ageFactor == 2) 
  
meanAge <- aggregate(PTMIBRTD ~ yearMonth,data= youngFemale , FUN = mean)
varData <- aggregate(cbind(selfHarm+suicide,female,arcs1,arcs2,arcs3,arcs4,arcs5,arcs6,arcs7,arcs8,
                           iukd10,iukd20,iukd30,iukd51,iukd52,iukd60,iukd88,iukd99) ~ yearMonth,data= youngFemale, FUN = sum)
varData <- rename(varData,observed = V1 )
varData <- merge(varData, meanAge, by = "yearMonth")
varData <- varData[,-1]


timePoints<- seq.Date(as.Date("2016-01-01"),as.Date("2020-12-01"),by = "months")
data <- zoo(cbind(varData),timePoints)

pre.period <- as.Date(c("2016-01-01","2020-02-01"))
post.period <- as.Date(c("2020-03-01","2020-12-01"))

impact <- CausalImpact(data,pre.period,post.period)
plot(impact)
