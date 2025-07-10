
library(dplyr)
library(tidyverse)
library(lubridate)


kosis <- read.csv("./covid19 suicide/kosis_suicide.csv",stringsAsFactors = F)
nedis <- read.table("./covid19 suicide/nedis/nedis.txt",sep="^",header=T,na.strings=c("-",""))

####preprocessing####

nedis$PTMIEMRT <- as.factor(nedis$PTMIEMRT)
nedis$PTMIDCRT <- as.factor(nedis$PTMIDCRT)
nedis$PTMIDCDT <- as.Date(as.character(nedis$PTMIDCDT),"%Y%m%d")
nedis$PTMIOTDT <- as.Date(as.character(nedis$PTMIOTDT),"%Y%m%d")
nedis$PTMIINDT <- as.Date(as.character(nedis$PTMIINDT),"%Y%m%d")

## 시,군만 추출 ## 
nedis$emar <- as.numeric(str_sub(nedis$PTMIEMAR,1,2))
nedis$gucd <- as.numeric(str_sub(nedis$PTMIGUCD,1,2))



## Vector of categorical variables that need transformation
catVars <- c("emar","PTMIEMCL","PTMISEXX","gucd","PTMIIUKD","PTMIDGKD",
             "PTMIARCF","PTMIARCS","PTMIINRT","PTMIINMN","PTMIEMSY",
             "PTMIKTS1","PTMIKJOB","PTMIKTS2","PTMIAREA","PTMIEMRT","PTMIHSRT",
             "PTMIDCRT")


for (cat in catVars){
  nedis[,cat] <- as.factor(nedis[,cat])
}


## filtering ## 
# nedis <- nedis %>% 
#   filter(PTMIBRTD %!in% c(0.08,1:6)) %>%
#   # filter(PTMIINMN %!in% c(8,9)) %>%
#   # filter(PTMIAREA %!in% c(9)) %>%
#   # filter(PTMIHSRT %!in% c(14,15,16,17,18,23,24,25,26,28,88,99)) %>%
#   # filter(PTMIDCRT %!in% c(5,6,8)) %>%
#   # filter(PTMIEMRT %!in% c(18,28,29,38,48,88,99)) %>%
#   droplevels()

nedis$PTMIHSRT[nedis$PTMIHSRT %in% c(14,15,16,17,23,24,25,26,31)] <- NA
nedis$PTMIEMRT[nedis$PTMIEMRT==29] <- NA
nedis$PTMIKTS2[nedis$PTMIKTS2 == 8 |nedis$PTMIKTS2 == 9] <- NA

## make suicide column
nedis <- nedis %>% mutate(suicide = case_when((PTMIEMRT %in% c(41,42,43,44,45,48)) ~ 1,
                                              (PTMIEMRT %in% c(31,32,33,34,38)&PTMIDCRT == 4) ~ 1,
                                              TRUE ~ 0))


## make column for Pre-covid, Post-covid ## 
nedis$covid <- ifelse(nedis$PTMIINDT >= "2020-02-29",1,0)

## combining factors ## 
# 
# nedis <- nedis %>% 
#   mutate(IUKD = forcats::fct_collapse(PTMIIUKD, "1" = c("10"), "2" = c("51","52"), "3" = c("60"), "4" = c("20","30","88","99")))
# 
# nedis <- nedis %>%
#   mutate(EMAR = forcats::fct_collapse(emar, "1" = c("11","28","41"), "2" = c("30","36","43","44"), "3" = c("29","45","46"),
#                                       "4" = c("26","27","31","47","48"),"5" = c("42"),"6"=c("50")))
# 
nedis <- nedis %>%
  mutate(ARCS = forcats::fct_collapse(PTMIARCS, "1" = c("1","3","4","8","9"), "2" = c("10"), "3" = c("30"), "4" = c("60"),
                                      "5" = c("70"),"6"=c("80"),"7" = c("11","20","40","50","88"),"8" = c("99")))

nedis <- nedis %>%
  mutate(INMN = forcats::fct_collapse(PTMIINMN, "1" = c("1","2","3","4","6"),"2"=c("5"),"3"=c("7")))


nedis <- nedis %>%
  mutate(EMRT = forcats::fct_collapse(PTMIEMRT, "1" = c("11","12","13","14","15","18"), "2" = c("21","22","23","24","25","26","27","28"), 
                                      "3" = c("31","32","33","34","38"), "4" = c("41","42","43","44","45","48")))

# #nedis <- nedis %>% 
# #  mutate(HSRT = forcats::fct_collapse(PTMIHSRT, "1" = c("11"), "2" = c("12","13"), "3" = c("21","22")))
# 
# nedis <- nedis %>%
#   mutate(GUCD = fct_collapse(gucd, "1" = c("11","28","41"), "2" = c("30","36","43","44"), "3" = c("29","45","46"),
#                              "4" = c("26","27","31","47","48"),"5" = c("42"),"6"=c("50")))

## remove unnecessary factor levels ##
nedis <- nedis %>%
  droplevels()

nedis$date<-ymd(nedis$PTMIAKDT)

nrow(nedis) #160044

nedis <- nedis %>% 
  filter(month(.data$date) %in% 1:12) %>%
  filter(year(.data$date) %in% 2016:2020); nrow(nedis) #159512

nedis$year <- strftime(nedis$date, "%Y")
nedis$year <- factor(nedis$year)


# ## Vector of variables to summarize
# myVars <- c("emar","PTMIEMCL","PTMISEXX","PTMIBRTD","gucd","PTMIIUKD","ARCS","PTMIINRT","INMN","PTMIEMSY",
#             "PTMIKTS1","PTMIKJOB","PTMIKTS2","PTMIAREA","EMRT","PTMIHSRT",
#             "PTMIDCRT","suicide")
# 
# ## Vector of categorical variables that need transformation
# catVars <- c("emar","PTMIEMCL","PTMISEXX","gucd","PTMIIUKD","ARCS","PTMIINRT","INMN","PTMIEMSY",
#              "PTMIKTS1","PTMIKJOB","PTMIKTS2","PTMIAREA","EMRT","PTMIHSRT",
#              "PTMIDCRT","suicide")
# 
# 
# tab2 <- tableone::CreateTableOne(vars = myVars, data = nedis,strata="year",
#                                  factorVars = catVars, addOverall = T)
# aa <- print(tab2,showAllLevels = TRUE)
# 
# #write.csv(aa, file = "C:/Users/me200v1wdp030/Desktop/covid19 suicide/myTable.csv")

nedis$selfHarm <- ifelse(nedis$suicide==1, 0, 1)

nedis$yearMonth <- strftime(nedis$date, "%Y%m")
nedis$yearMonth <- factor(nedis$yearMonth)


female <- nedis %>%
  filter(PTMISEXX == "F")

male <- nedis %>%
  filter(PTMISEXX == "M")
