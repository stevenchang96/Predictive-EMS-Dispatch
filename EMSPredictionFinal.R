library(lubridate)
library(RSocrata)
library(tidyverse)

#Let's learn how to wrangle a time series dataset in R. Note that we are going to look at count of trips
#by station by hour. If you do this model for your project, you do not want to model count, but 
#CAPACITY.

#check out all that we can do with lubridate
#https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf
time <- ymd_hms("2010-12-13 15:30:30")

#read in some bike share data
dat <- read.socrata("https://data.chattlibrary.org/Transportation/Bike-Chattanooga-Trip-Data/8yba-nwv8")

View(dat)

#what is the date format?
head(dat$StartDateTime)

#get just 2017 trips
dat_17 <- 
  dat %>%
  mutate(dateTime2 = ymd_hms(StartDateTime)) %>%
  filter(year(dateTime2) == "2017",
         month(dateTime2) == "7") 

#trip frequency
dat_17 %>%
  group_by(StartStationID) %>%
  count()

#How do I get a count of trips by hour?
dat_17 %>%
  #get only the 29th week (its in July)
  filter(week(dateTime2)== "29") %>%
  #Create a string field of day of the week
  mutate(dotw = wday(dateTime2,label=TRUE)) %>%
  #filter out weekdays
  filter(dotw != "Sun" & dotw != "Sat") %>%
  #mutate the hour of the day
  mutate(hour = hour(dateTime2)) %>%
  #Get count of trips by station id, day of the week and hour
  group_by(StartStationID,dotw,hour) %>%
  count() %>%
  #sort
  arrange(dotw,-hour,StartStationID) %>%
  #output
  as.data.frame() %>%
  head(20)

#How about a time series plot for the whole month by hour and day of the week including weekends.
dat_17 %>%
  mutate(dotw = wday(dateTime2,label=TRUE)) %>%
  #filter(dotw != "Sun" & dotw != "Sat") %>%
  mutate(hour = hour(dateTime2)) %>%
  group_by(dotw,hour) %>%
  count() %>%
  ggplot(aes(hour,n)) +
  geom_point() +
  geom_line() +
  facet_wrap(~dotw)

#How about a regression - this time only for 1 week of the data. Week 29. 
dat_17 %>%
  filter(week(dateTime2) == "29") %>%
  mutate(dotw = wday(dateTime2,label=TRUE)) %>%
  filter(dotw != "Sun" & dotw != "Sat") %>%
  mutate(hour = hour(dateTime2)) %>%
  group_by(StartStationID,dotw,hour) %>%
  count() %>%
  ungroup() %>%
  select(-StartStationID) %>%    
  lm(log(n) ~ ., data=.) %>%
  summary()

#How about we bin by 6 hour time periods
#See https://stackoverflow.com/questions/50304159/label-day-timing-into-morning-afternoon-and-evening-in-r
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")

#regress with day of the week
dat_17 %>%
  filter(week(dateTime2) == "29") %>%
  mutate(dotw = wday(dateTime2,label=TRUE)) %>%
  filter(dotw != "Sun" & dotw != "Sat") %>%
  mutate(hour = hour(dateTime2)) %>%
  #create 4 new time fixed effects
  mutate(Time_of_day = cut(x=hour(dateTime2), breaks = breaks, labels = labels, include.lowest=TRUE)) %>%
  group_by(StartStationID, dotw, Time_of_day) %>%
  count() %>%
  ungroup() %>%
  select(-StartStationID) %>%
  lm(log(n) ~ ., data= .) %>%
  summary()

#Now create a full data frame of hours, add station fixed effects to the regression.
#create a data fraf trip count by station, day of the week and hour, then regress
dat_17.hour <-
  dat_17 %>%
  filter(week(dateTime2) == "29") %>%
  mutate(dotw = wday(dateTime2,label=TRUE)) %>%
  filter(dotw != "Sun" & dotw != "Sat") %>%
  mutate(hour = hour(dateTime2)) %>%
  group_by(StartStationID,dotw,hour) %>%
  count() %>%
  ungroup() %>%
  #convert station id to factor so we can regress on it
  mutate(StartStationID = as.factor(StartStationID))

#regress
reg <- lm(log(n) ~ ., data= dat_17.hour)
summary(reg)

#create a data frame of predicted and observed. Note the long form.
predicted_data.frame <-
  data.frame(
    observed = dat_17.hour$n,
    predicted = exp(predict(reg,dat_17.hour)),
    hour = dat_17.hour$hour,
    dotw = dat_17.hour$dotw,
    station = dat_17.hour$StartStationID) %>%
  #select just station 1319
  filter(station == "1319") %>%
  gather(key,value, -hour,-dotw, -station)

ggplot(predicted_data.frame, aes(hour,value,group=key,colour=key)) +
  geom_point() +
  geom_line() +
  facet_wrap(~dotw) +
  labs(title = "Predicted and observed trips for station 1329")

#what do you think about the distribution of counts?

dat_17 %>%
  filter(week(dateTime2) == "29") %>%
  mutate(dotw = wday(dateTime2,label=TRUE)) %>%
  filter(dotw != "Sun" & dotw != "Sat") %>%
  mutate(hour = hour(dateTime2)) %>%
  #create 4 new time fixed effects
  mutate(Time_of_day = cut(x=hour(dateTime2), breaks = breaks, labels = labels, include.lowest=TRUE)) %>%
  group_by(StartStationID, dotw, Time_of_day) %>%
  count() %>%
  ungroup() %>%
  ggplot(aes(n)) + 
  geom_histogram()
