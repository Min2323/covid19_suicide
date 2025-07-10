library(dplyr)
library(qcc)
library(season)
library(Hmisc)

nedis <- nedis %>% mutate(ageFactor = case_when((PTMIBRTD < 18) ~ 1,
                                                (PTMIBRTD >= 18 & PTMIBRTD < 30) ~ 2,
                                                (PTMIBRTD >= 30 & PTMIBRTD < 60) ~ 3,
                                                (PTMIBRTD >= 60) ~ 4 ))

nedis$ageFactor <- as.factor(nedis$ageFactor)

nedis$year <- strftime(nedis$date, "%Y")
nedis$year <- as.numeric(nedis$year)
nedis$month <- strftime(nedis$date, "%m")
nedis$month <- as.numeric(nedis$month)

##### total #####
### suicide ###

varData <- aggregate(suicide~year+month,data=nedis,FUN = sum)
varData <- arrange(varData, year,month)

# mutate post-covid column in varData
varData <- varData %>%
  mutate(covid = ifelse(year == 2020 & month >= 3,1,0))

varData$time <- index(varData)
#varData <- varData[,-1]

#fit poisson regression(no adjustment)

fit1 <- glm(suicide ~ covid+time,family = poisson, data = varData )
summary(fit1)
summary(fit1)$dispersion
round(ci.lin(fit1,Exp=T),3)

# calculate RR
ci.lin(fit1,Exp=T)["covid",4:7]

#overdispersion test
qcc::qcc.overdispersion.test(varData$suicide,type="poisson")

# fit poisson regression(seasonally adjusted model) 
fit2 <- glm(suicide ~ covid+time+harmonic(month,2,12),family = quasipoisson, data = varData )
summary(fit2)
summary(fit2)$dispersion
round(ci.lin(fit2,Exp=T),3)

# calculate RR
ci.lin(fit2,Exp=T)["covid",4:7]

# TREND
exp(coef(fit2)["time"]*12)

# We again check the model and autocorrelation functions
res3 <- residuals(fit2,type="deviance")
acf(res3)
pacf(res3)

#no autocorrelation

#predict and plot of the seasonally adjusted model

datanew <- data.frame(covid=rep(c(0,1),c(500,100)),
                      time= 1:600/10,month=rep(1:120/10,5))


pred <- predict(fit2,type="response",datanew)
plot(varData$suicide,type="n",xlab="Year",ylab="suicide",
     bty="l",xaxt="n")
rect(50,80,60,220,col=grey(0.9),border=F)
points(varData$suicide,cex=0.7)
axis(1,at=0:5*12,labels=F)
axis(1,at=0:4*12+6,tick=F,labels=2016:2020)
lines(1:600/10,pred,col=2)
title("suicide, 2016-2020")

### selfharm ###

varData <- aggregate(selfHarm~year+month,data=nedis,FUN = sum)
varData <- arrange(varData, year,month)

# mutate post-covid column in varData
varData <- varData %>%
  mutate(covid = ifelse(year == 2020 & month >= 3,1,0))

varData$time <- index(varData)
#varData <- varData[,-1]

#fit poisson regression(no adjustment)

fit1 <- glm(selfHarm ~ covid+time,family = poisson, data = varData )
summary(fit1)
summary(fit1)$dispersion
round(ci.lin(fit1,Exp=T),3)

# calculate RR
ci.lin(fit1,Exp=T)["covid",4:7]

#overdispersion test
qcc::qcc.overdispersion.test(varData$selfHarm,type="poisson")

# fit poisson regression(seasonally adjusted model) 
fit2 <- glm(selfHarm ~ covid+time+harmonic(month,2,12),family = quasipoisson, data = varData )
summary(fit2)
summary(fit2)$dispersion
round(ci.lin(fit2,Exp=T),3)

# calculate RR
ci.lin(fit2,Exp=T)["covid",4:7]

# TREND
exp(coef(fit2)["time"]*12)

# We again check the model and autocorrelation functions
res3 <- residuals(fit2,type="deviance")
acf(res3)
pacf(res3)


#predict and plot of the seasonally adjusted model

datanew <- data.frame(covid=rep(c(0,1),c(500,100)),
                      time= 1:600/10,month=rep(1:120/10,5))


pred <- predict(fit2,type="response",datanew)
plot(varData$selfHarm,type="n",xlab="Year",ylab="suicide",
     bty="l",xaxt="n")
rect(50,1650,60,3300,col=grey(0.9),border=F)
points(varData$selfHarm,cex=0.7)
axis(1,at=0:5*12,labels=F)
axis(1,at=0:4*12+6,tick=F,labels=2016:2020)
lines(1:600/10,pred,col=2)
title("self-harm, 2016-2020")


### selfharm + suicide ###

varData <- aggregate(selfHarm+suicide~year+month,data=nedis,FUN = sum)
varData <- rename(varData,observed = `selfHarm + suicide` )
varData <- arrange(varData, year,month)

# mutate post-covid column in varData
varData <- varData %>%
  mutate(covid = ifelse(year == 2020 & month >= 3,1,0))

varData$time <- index(varData)
#varData <- varData[,-1]

#fit poisson regression(no adjustment)

fit1 <- glm(observed~ covid+time,family = poisson, data = varData )
summary(fit1)
summary(fit1)$dispersion
round(ci.lin(fit1,Exp=T),3)

# calculate RR
ci.lin(fit1,Exp=T)["covid",4:7]

#overdispersion test
qcc::qcc.overdispersion.test(varData$observed,type="poisson")

# fit poisson regression(seasonally adjusted model) 
fit2 <- glm(observed ~ covid+time+harmonic(month,2,12),family = quasipoisson, data = varData )
summary(fit2)
summary(fit2)$dispersion
round(ci.lin(fit2,Exp=T),3)

# calculate RR
ci.lin(fit2,Exp=T)["covid",4:7]

# TREND
exp(coef(fit2)["time"]*12)

# We again check the model and autocorrelation functions
res3 <- residuals(fit2,type="deviance")
acf(res3)
pacf(res3)


#predict and plot of the seasonally adjusted model

datanew <- data.frame(covid=rep(c(0,1),c(500,100)),
                      time= 1:600/10,month=rep(1:120/10,5))


pred <- predict(fit2,type="response",datanew)
plot(varData$observed,type="n",xlab="Year",ylab="suicide",
     bty="l",xaxt="n")
rect(50,1800,60,3500,col=grey(0.9),border=F)
points(varData$observed,cex=0.7)
axis(1,at=0:5*12,labels=F)
axis(1,at=0:4*12+6,tick=F,labels=2016:2020)
lines(1:600/10,pred,col=2)
title("suicide + self-harm, 2016-2020")


##### young Female #####
youngFemale <- nedis %>%
  filter(PTMISEXX == "F") %>%
  filter(ageFactor == 2)

### suicide ###

varData <- aggregate(suicide~year+month,data=youngFemale,FUN = sum)
varData <- arrange(varData, year,month)

# mutate post-covid column in varData
varData <- varData %>%
  mutate(covid = ifelse(year == 2020 & month >= 3,1,0))

varData$time <- index(varData)
#varData <- varData[,-1]

#fit poisson regression(no adjustment)

fit1 <- glm(suicide ~ covid+time,family = poisson, data = varData )
summary(fit1)
summary(fit1)$dispersion
round(ci.lin(fit1,Exp=T),3)

# calculate RR
ci.lin(fit1,Exp=T)["covid",4:7]

#overdispersion test
qcc::qcc.overdispersion.test(varData$suicide,type="poisson")

# fit poisson regression(seasonally adjusted model) 
fit2 <- glm(suicide ~ covid+time+harmonic(month,2,12),family = quasipoisson, data = varData )
summary(fit2)
summary(fit2)$dispersion
round(ci.lin(fit2,Exp=T),3)

# calculate RR
ci.lin(fit2,Exp=T)["covid",4:7]

# TREND
exp(coef(fit2)["time"]*12)

# We again check the model and autocorrelation functions
res3 <- residuals(fit2,type="deviance")
acf(res3)
pacf(res3)

#no autocorrelation

#predict and plot of the seasonally adjusted model

datanew <- data.frame(covid=rep(c(0,1),c(500,100)),
                      time= 1:600/10,month=rep(1:120/10,5))


pred <- predict(fit2,type="response",datanew)
plot(varData$suicide,type="n",xlab="Year",ylab="suicide",
     bty="l",xaxt="n")
rect(50,0,60,30,col=grey(0.9),border=F)
points(varData$suicide,cex=0.7)
axis(1,at=0:5*12,labels=F)
axis(1,at=0:4*12+6,tick=F,labels=2016:2020)
lines(1:600/10,pred,col=2)
title("suicide, young female, 2016-2020")

### selfharm ###

varData <- aggregate(selfHarm~year+month,data=youngFemale,FUN = sum)
varData <- arrange(varData, year,month)

# mutate post-covid column in varData
varData <- varData %>%
  mutate(covid = ifelse(year == 2020 & month >= 3,1,0))

varData$time <- index(varData)
#varData <- varData[,-1]

#fit poisson regression(no adjustment)

fit1 <- glm(selfHarm ~ covid+time,family = poisson, data = varData )
summary(fit1)
summary(fit1)$dispersion
round(ci.lin(fit1,Exp=T),3)

# calculate RR
ci.lin(fit1,Exp=T)["covid",4:7]

#overdispersion test
qcc::qcc.overdispersion.test(varData$selfHarm,type="poisson")

# fit poisson regression(seasonally adjusted model) 
fit2 <- glm(selfHarm ~ covid+time+harmonic(month,2,12),family = quasipoisson, data = varData )
summary(fit2)
summary(fit2)$dispersion
round(ci.lin(fit2,Exp=T),3)

# calculate RR
ci.lin(fit2,Exp=T)["covid",4:7]

# TREND
exp(coef(fit2)["time"]*12)

# We again check the model and autocorrelation functions
res3 <- residuals(fit2,type="deviance")
acf(res3)
pacf(res3)


#predict and plot of the seasonally adjusted model

datanew <- data.frame(covid=rep(c(0,1),c(500,100)),
                      time= 1:600/10,month=rep(1:120/10,5))


pred <- predict(fit2,type="response",datanew)
plot(varData$selfHarm,type="n",xlab="Year",ylab="suicide",
     bty="l",xaxt="n")
rect(50,200,60,850,col=grey(0.9),border=F)
points(varData$selfHarm,cex=0.7)
axis(1,at=0:5*12,labels=F)
axis(1,at=0:4*12+6,tick=F,labels=2016:2020)
lines(1:600/10,pred,col=2)
title("self-harm, young female, 2016-2020")


### selfharm + suicide ###

varData <- aggregate(selfHarm+suicide~year+month,data=youngFemale,FUN = sum)
varData <- rename(varData,observed = `selfHarm + suicide` )
varData <- arrange(varData, year,month)

# mutate post-covid column in varData
varData <- varData %>%
  mutate(covid = ifelse(year == 2020 & month >= 3,1,0))

varData$time <- index(varData)
#varData <- varData[,-1]

#fit poisson regression(no adjustment)

fit1 <- glm(observed~ covid+time,family = poisson, data = varData )
summary(fit1)
summary(fit1)$dispersion
round(ci.lin(fit1,Exp=T),3)

# calculate RR
ci.lin(fit1,Exp=T)["covid",4:7]

#overdispersion test
qcc::qcc.overdispersion.test(varData$observed,type="poisson")

# fit poisson regression(seasonally adjusted model) 
fit2 <- glm(observed ~ covid+time+harmonic(month,2,12),family = quasipoisson, data = varData )
summary(fit2)
summary(fit2)$dispersion
round(ci.lin(fit2,Exp=T),3)

# calculate RR
ci.lin(fit2,Exp=T)["covid",4:7]

# TREND
exp(coef(fit2)["time"]*12)

# We again check the model and autocorrelation functions
res3 <- residuals(fit2,type="deviance")
acf(res3)
pacf(res3)


#predict and plot of the seasonally adjusted model

datanew <- data.frame(covid=rep(c(0,1),c(500,100)),
                      time= 1:600/10,month=rep(1:120/10,5))


pred <- predict(fit2,type="response",datanew)
plot(varData$observed,type="n",xlab="Year",ylab="suicide",
     bty="l",xaxt="n")
rect(50,200,60,860,col=grey(0.9),border=F)
points(varData$observed,cex=0.7)
axis(1,at=0:5*12,labels=F)
axis(1,at=0:4*12+6,tick=F,labels=2016:2020)
lines(1:600/10,pred,col=2)
title("suicide + self-harm, young female, 2016-2020")
