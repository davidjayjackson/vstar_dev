library(tidyverse)
library(openxlsx)
library(plotly)

rm(list=ls())
mydata <-data.table::fread("../db/alf_ori.csv") %>% 
  select(JD,Star_Name,Band,Magnitude,Validation_Flag) %>%
  mutate( JD = as.numeric(JD)) %>%
  mutate(Ymd =insol::JD(JD,inverse = TRUE)) %>%
  mutate(Ymd = as.Date(Ymd))  %>%
  mutate(Magnitude = as.numeric( gsub("<","", Magnitude) ) + 0.99) %>%
    filter(Band=="Vis." & Validation_Flag =="Z" )
mydata$Star_Name <- "ALF_ORI"
mydata %>% group_by(Star_Name) %>% summarise(n())
mydata %>% group_by(Band) %>% summarise(n())
mydata %>% group_by(Validation_Flag) %>% summarise(n())

## Fun with Moving Averages
mydata$MA <- forecast::ma(mydata$Magnitude,order=3)
mydata$MA <- round(mydata$MA,digits = 1)
mydata$Plus <- round(mydata$MA +1,digits = 1)
mydata$Minus <- round(mydata$MA -1,digits = 1)
starDust <- mydata %>%
    group_by(Ymd) %>%
    summarize(Mean = round(mean(Magnitude),digits=1),
              Brighter = round(Mean -1,digits = 1),
              Fainter = round(Mean +1,digits= 1),
              Obs = n()) %>% 
    ungroup() %>% 
    mutate(Verify = case_when(
      (Brighter <  Mean -1) | (Fainter > Mean + 1) ~ "Yes",TRUE ~ "No")) %>%
      filter(Obs >=3 & Verify =="Yes")



g <- ggplot() + geom_point(data=mydata,aes(x=Ymd,y=Magnitude,color=Validation_Flag)) +
  geom_line(data=mydata,aes(x=Ymd,y=Minus,color="Mean -1")) +
  geom_line(data=mydata,aes(x=Ymd,y=Plus,color="Mean +1")) +
  scale_y_reverse() ggitle("ALF_ORI 3 Day Moving Average")

ggplotly(g)

g1 <- ggplot() + geom_point(data=mydata,aes(x=Ymd,y=Magnitude,color=Validation_Flag)) +
  geom_line(data=starDust,aes(x=Ymd,y=Brighter,color="Mean -1")) +
  geom_line(data=starDust,aes(x=Ymd,y=Fainter,color="Mean +1")) +
  scale_y_reverse() + ggtitle("ALF_ORI: Mean +/- 1")

ggplotly(g1)
# write.xlsx(starDust,file="ucyg.xlsx",colNames=TRUE)
