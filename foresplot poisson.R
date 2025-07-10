library(dplyr)
library(ggplot2)
library(qcc)

## suicide ##
nedis <- nedis %>% mutate(ageFactor = case_when((PTMIBRTD < 18) ~ 1,
                                                (PTMIBRTD >= 18 & PTMIBRTD < 30) ~ 2,
                                                (PTMIBRTD >= 30 & PTMIBRTD < 60) ~ 3,
                                                (PTMIBRTD >= 60) ~ 4 ))

nedis$ageFactor <- as.factor(nedis$ageFactor)


preCovid <- nedis %>%
  filter(year != 2020)

postCovid <- nedis %>%
  filter(year == 2020)

postCovidData <- aggregate(suicide~PTMISEXX+ageFactor,data=postCovid,FUN = sum)

varData <- aggregate(suicide~year+PTMISEXX+ageFactor,data=preCovid,FUN = sum)
varData <- varData[,-1]
qcc::qcc.overdispersion.test(varData$suicide,type="poisson")

fit <- glm(suicide ~ .,family = quasipoisson(link = log), data = varData )

newdat <- unique(varData[,-3])


pred <- predict(fit, newdata = newdat, type = "response",se.fit=T)

newdat$predict <- pred$fit

newdat <- merge(newdat,postCovidData,by=c("PTMISEXX","ageFactor"))

newdat <- newdat %>%
  mutate(age = case_when((ageFactor == 1) ~ "< 18",
                         (ageFactor == 2) ~ "18-29",
                         (ageFactor == 3) ~ "30-59",
                         (ageFactor == 4) ~ ">60"))


newdat$se <- sqrt(1/newdat$suicide)
newdat$RR <- newdat$suicide / newdat$predict
newdat$lower <- exp(log(newdat$RR) - 1.96 * newdat$se)
newdat$upper <- exp(log(newdat$RR) + 1.96 * newdat$se)
newdat$index <- nrow(newdat):1


ggplot(data=newdat, aes(y=index, x=RR, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) + xlab("") + ylab("") +
  scale_y_continuous(name = "", breaks=1:nrow(newdat)) + geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  theme_classic() + theme(axis.text.y=element_blank())


## self-harm ##
nedis <- nedis %>% mutate(ageFactor = case_when((PTMIBRTD < 18) ~ 1,
                                                (PTMIBRTD >= 18 & PTMIBRTD < 30) ~ 2,
                                                (PTMIBRTD >= 30 & PTMIBRTD < 60) ~ 3,
                                                (PTMIBRTD >= 60) ~ 4 ))

nedis$ageFactor <- as.factor(nedis$ageFactor)


preCovid <- nedis %>%
  filter(year != 2020)

postCovid <- nedis %>%
  filter(year == 2020)

postCovidData <- aggregate(selfHarm~PTMISEXX+ageFactor,data=postCovid,FUN = sum)

varData <- aggregate(selfHarm~year+PTMISEXX+ageFactor,data=preCovid,FUN = sum)
varData <- varData[,-1]
qcc::qcc.overdispersion.test(varData$selfHarm,type="poisson")

fit <- glm(selfHarm ~ .,family = quasipoisson(link = log), data = varData )

newdat <- unique(varData[,-3])


pred <- predict(fit, newdata = newdat, type = "response",se.fit=T)

newdat$predict <- pred$fit

newdat <- merge(newdat,postCovidData,by=c("PTMISEXX","ageFactor"))

newdat <- newdat %>%
  mutate(age = case_when((ageFactor == 1) ~ "< 18",
                         (ageFactor == 2) ~ "18-29",
                         (ageFactor == 3) ~ "30-59",
                         (ageFactor == 4) ~ ">60"))


newdat$se <- sqrt(1/newdat$selfHarm)
newdat$RR <- newdat$selfHarm / newdat$predict
newdat$lower <- exp(log(newdat$RR) - 1.96 * newdat$se)
newdat$upper <- exp(log(newdat$RR) + 1.96 * newdat$se)
newdat$index <- nrow(newdat):1


ggplot(data=newdat, aes(y=index, x=RR, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) + xlab("") + ylab("") +
  scale_y_continuous(name = "", breaks=1:nrow(newdat)) + geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  theme_classic() + theme(axis.text.y=element_blank())

## suicide + self-harm ## 
nedis <- nedis %>% mutate(ageFactor = case_when((PTMIBRTD < 18) ~ 1,
                                                (PTMIBRTD >= 18 & PTMIBRTD < 30) ~ 2,
                                                (PTMIBRTD >= 30 & PTMIBRTD < 60) ~ 3,
                                                (PTMIBRTD >= 60) ~ 4 ))

nedis$ageFactor <- as.factor(nedis$ageFactor)


preCovid <- nedis %>%
  filter(year != 2020)

postCovid <- nedis %>%
  filter(year == 2020)

postCovidData <- aggregate(suicide+selfHarm~PTMISEXX+ageFactor,data=postCovid,FUN = sum)
postCovidData <- rename(postCovidData,observed = `suicide + selfHarm`)

varData <- aggregate(suicide+selfHarm~year+PTMISEXX+ageFactor,data=preCovid,FUN = sum)
varData <- rename(varData,observed = `suicide + selfHarm`)
varData <- varData[,-1]
qcc::qcc.overdispersion.test(varData$observed,type="poisson")

fit <- glm(observed ~ .,family = quasipoisson(link = log), data = varData )

newdat <- unique(varData[,-3])


pred <- predict(fit, newdata = newdat, type = "response",se.fit=T)

newdat$predict <- pred$fit

newdat <- merge(newdat,postCovidData,by=c("PTMISEXX","ageFactor"))

newdat <- newdat %>%
  mutate(age = case_when((ageFactor == 1) ~ "< 18",
                         (ageFactor == 2) ~ "18-29",
                         (ageFactor == 3) ~ "30-59",
                         (ageFactor == 4) ~ ">60"))


newdat$se <- sqrt(1/newdat$observed)
newdat$RR <- newdat$observed / newdat$predict
newdat$lower <- exp(log(newdat$RR) - 1.96 * newdat$se)
newdat$upper <- exp(log(newdat$RR) + 1.96 * newdat$se)
newdat$index <- nrow(newdat):1


ggplot(data=newdat, aes(y=index, x=RR, xmin=lower, xmax=upper)) +
  geom_point() + 
  geom_errorbarh(height=.1) + xlab("") + ylab("") +
  scale_y_continuous(name = "", breaks=1:nrow(newdat)) + geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
  theme_classic() + theme(axis.text.y=element_blank())

