#pure R code

library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(magrittr)
library(gridExtra)


d_activity    <- read_csv("data/dailyActivity_merged.csv")
d_calories    <- read.csv("data/dailyCalories_merged.csv")
d_intensities <- read.csv("data/dailyIntensities_merged.csv")
d_steps       <- read.csv("data/dailySteps_merged.csv")
s_heartrate   <- read.csv("data/heartrate_seconds_merged.csv")
h_calories    <- read.csv("data/hourlyCalories_merged.csv")
h_intensitie  <- read.csv("data/hourlyIntensities_merged.csv")
h_steps       <- read.csv("data/hourlySteps_merged.csv")
kable(head(d_activity))
kable(head(d_calories))
kable(head(d_steps))
kable(head(d_intensities))
identical(as.integer(d_activity[['Calories']]),d_calories[['Calories']])
identical(as.integer(d_activity[['TotalSteps']]),d_steps[['StepTotal']])
identical(as.integer(d_activity[['SedentaryMinutes']]),d_intensities[['SedentaryMinutes']])
identical(as.integer(d_activity[['LightlyActiveMinutes']]),d_intensities[['LightlyActiveMinutes']])
identical(as.integer(d_activity[['FairlyActiveMinutes']]),d_intensities[['FairlyActiveMinutes']])
identical(as.integer(d_activity[['VeryActiveMinutes']]),d_intensities[['VeryActiveMinutes']])
identical(d_activity[['SedentaryActiveDistance']],d_intensities[['SedentaryActiveDistance']])
identical(d_activity[['LightActiveDistance']],d_intensities[['LightActiveDistance']])
identical(d_activity[['ModeratelyActiveDistance']],d_intensities[['ModeratelyActiveDistance']])
identical(d_activity[['VeryActiveDistance']],d_intensities[['VeryActiveDistance']])
d_activity$ActivityDate<-as.Date(d_activity$ActivityDate, "%m/%d/%Y")
d_activity$Id<-as.character(d_activity$Id)
h_intensitie$ActivityHour<-as.POSIXct(h_intensitie$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
h_intensitie$Time<-format(h_intensitie$ActivityHour, format = "%H:%M:%S")
h_intensitie$Date<-as.Date(h_intensitie$ActivityHour, format = "%m/%d/%y")
h_intensitie <- subset(h_intensitie, select = -c(ActivityHour) )
h_intensitie$Id<-as.character(h_intensitie$Id)
n_distinct(d_activity$Id)
n_distinct(d_activity$ActivityDate)
d_activity %>%  
  select(TotalSteps, TotalDistance, TrackerDistance, LoggedActivitiesDistance, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()
ggplot(data=d_activity)+
  geom_point(mapping=aes(x=ActivityDate, y=TotalDistance)) +
  geom_smooth(mapping=aes(x=ActivityDate, y=TotalDistance)) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20)) +
  ggtitle("Total distance by date")+
  xlab("Time")+
  ylab("Distance")
ggplot(data=d_activity)+
  geom_point(mapping=aes(x=ActivityDate, y=Calories)) +
  geom_smooth(mapping=aes(x=ActivityDate, y=Calories)) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20))+
  ggtitle("Potential decrease in calories over time")+
  xlab("Time")+
  ylab("Total calories")
c_plot1<-ggplot(data=d_activity)+
  geom_point(mapping=aes(x=ActivityDate, y=Calories)) +
  geom_smooth(mapping=aes(x=ActivityDate, y=Calories)) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20))+
  ggtitle("Total calories by all dates\n")+
  xlab("Time")+
  ylab("Total calories")

c_plot2<-ggplot(data=d_activity)+
  geom_point(mapping=aes(x=ActivityDate, y=Calories)) +
  geom_smooth(mapping=aes(x=ActivityDate, y=Calories)) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20))+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-11'))+
  ggtitle("Total calories by date \nexcept for the last")+
  xlab("Time")+
  ylab("Total calories")

grid.arrange(c_plot1, c_plot2, ncol=2)
diff_int_plot <- ggplot(data=d_activity)+
  geom_smooth(mapping=aes(x=ActivityDate, y=LightlyActiveMinutes, colour="Light Activity")) +
  geom_point(mapping=aes(x=ActivityDate, y=LightlyActiveMinutes, colour="Light Activity"))+
  geom_smooth(mapping=aes(x=ActivityDate, y=FairlyActiveMinutes, colour="Medium Activity")) +
  geom_point(mapping=aes(x=ActivityDate, y=FairlyActiveMinutes, colour="Medium Activity")) +
  geom_smooth(mapping=aes(x=ActivityDate, y=VeryActiveMinutes, colour="Heavy Activty")) +
  geom_point(mapping=aes(x=ActivityDate, y=VeryActiveMinutes, colour="Heavy Activty")) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20))+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-11'))+
  theme_light()+
  ggtitle("Different intensities of excersise")+
  xlab("Time")+
  ylab("Minutes of activity")

diff_int_plot

ggplot(data=d_activity)+
  geom_point(mapping=aes(x=ActivityDate, y=Id, color=Id)) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20))+
  ggtitle("Activities logged per ID number")+
  xlab("Time")+
  ylab("ID numbers")+
  
  n_distinct(d_activity$Id)
#creating an empty data frame for the active people
mat = matrix(ncol = 0, nrow = 1)
active_people=data.frame(mat)
last_date<- as.Date('2016-05-11')
ii=1
for(i in 1:nrow(d_activity)) { 
  if(d_activity[i,2] > last_date){
    active_people[ii]<- d_activity[i,1]
    ii<-ii+1}
}

length(active_people)
#using the ID of the active people to create new variables
activity_in_use<- filter(d_activity, Id %in% active_people)
activity_not_in_use<- filter(d_activity, !(Id %in% active_people))
plot1<-ggplot(data=activity_in_use)+
  geom_point(mapping=aes(x=ActivityDate, y=Calories))+
  geom_smooth(mapping=aes(x=ActivityDate, y=Calories))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 20))+
  coord_cartesian(ylim = c(0, 4500))+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-10')) +
  ggtitle("Active people \nCalories over time")+
  xlab("Time")+
  ylab("Calories")

plot2<-  ggplot(data=activity_not_in_use)+
  geom_point(mapping=aes(x=ActivityDate, y=Calories))+
  geom_smooth(mapping=aes(x=ActivityDate, y=Calories))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 20))+
  coord_cartesian(ylim = c(0, 4500))+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-10'))+
  ggtitle("Non-active people \nCalories over time")+
  xlab("Time")+
  ylab("Calories")

plot3<-ggplot(data=activity_in_use)+
  geom_point(mapping=aes(x=ActivityDate, y=TotalSteps))+
  geom_smooth(mapping=aes(x=ActivityDate, y=TotalSteps))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 20))+
  coord_cartesian(ylim = c(0, 25000))+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-10'))+
  ggtitle("Active people \nTotal steps over time")+
  xlab("Time")+
  ylab("Steps")


plot4<-  ggplot(data=activity_not_in_use)+
  geom_point(mapping=aes(x=ActivityDate, y=TotalSteps))+
  geom_smooth(mapping=aes(x=ActivityDate, y=TotalSteps))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 20))+
  coord_cartesian(ylim = c(0, 25000))+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-10'))+
  ggtitle("Non-active people \nTotal steps over time")+
  xlab("Time")+
  ylab("Steps") 

plot5<- ggplot(data=activity_in_use)+
  geom_point(mapping=aes(x=ActivityDate, y=VeryActiveMinutes, colour="Heavy Activity")) +
  geom_point(mapping=aes(x=ActivityDate, y=FairlyActiveMinutes, colour="Medium Activity")) +
  geom_smooth(mapping=aes(x=ActivityDate, y=FairlyActiveMinutes, colour="Medium Activity")) +
  geom_smooth(mapping=aes(x=ActivityDate, y=VeryActiveMinutes, colour="Heavy Activity")) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20))+
  theme(legend.position = c(0.78, 0.86) ,legend.background = element_blank(), legend.box.background = element_rect(colour = "black"))+
  coord_cartesian(ylim = c(0, 150))+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-10'))+
  ggtitle("Active people \nMedium and Heavy \nactivity")+
  xlab("Time")+
  ylab("Minutes of activity") 

plot6<- ggplot(data=activity_not_in_use)+
  geom_point(mapping=aes(x=ActivityDate, y=VeryActiveMinutes, colour="Heavy Activity")) +
  geom_point(mapping=aes(x=ActivityDate, y=FairlyActiveMinutes, colour="Medium Activity")) +
  geom_smooth(mapping=aes(x=ActivityDate, y=FairlyActiveMinutes, colour="Medium Activity")) +
  geom_smooth(mapping=aes(x=ActivityDate, y=VeryActiveMinutes, colour="Heavy Activity")) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20))+
  theme(legend.position = c(0.78, 0.86),legend.background = element_blank(), legend.box.background = element_rect(colour = "black") )+
  coord_cartesian(ylim = c(0, 150))+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-10'))+
  ggtitle("Non-active people \nMedium and Heavy \nactivity")+
  xlab("Time")+
  ylab("Minutes of activity") 

grid.arrange(plot1, plot2, ncol=2)
grid.arrange(plot3, plot4, ncol=2)
grid.arrange(plot5, plot6, ncol=2)
ggplot(data=d_activity)+
  #geom_point(mapping=aes(x=ActivityDate, y=Calories, color=Id)) +
  geom_smooth( se=FALSE,mapping=aes(x=ActivityDate, y=TotalSteps, color=Id)) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20),legend.position = "none")+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-10'))+
  ggtitle("Total steps by date")+
  xlab("Time")+
  ylab("Total steps")

plot7<-ggplot(data=activity_in_use)+
  #geom_point(mapping=aes(x=ActivityDate, y=Calories, color=Id)) +
  geom_smooth( se=FALSE,mapping=aes(x=ActivityDate, y=TotalSteps, group=Id, color=Id)) +
  geom_smooth(mapping=aes(x=ActivityDate, y=TotalSteps)) +
  coord_cartesian(ylim = c(0, 20000))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 20),legend.position = "none")+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-10'))+
  ggtitle("Active group")+
  xlab("Time")+
  ylab("Total steps")

plot8<-ggplot(data=activity_not_in_use)+
  #geom_point(mapping=aes(x=ActivityDate, y=Calories, color=Id)) +
  geom_smooth( se=FALSE,mapping=aes(x=ActivityDate, y=TotalSteps, group=Id, color=Id)) +
  geom_smooth(mapping=aes(x=ActivityDate, y=TotalSteps)) +
  coord_cartesian(ylim = c(0, 20000))+
  theme_light()+
  theme(axis.text.x = element_text(angle = 20),legend.position = "none")+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-10'))+
  ggtitle("Non-active group")+
  xlab("Time")+
  ylab("Total steps")

grid.arrange(plot7, plot8, ncol=2, top="Individual steps per ID")
plot9<-ggplot(data=activity_in_use)+
  geom_smooth(se=FALSE,mapping=aes(x=ActivityDate, y=LightlyActiveMinutes, color=Id)) +
  geom_smooth(mapping=aes(x=ActivityDate, y=LightlyActiveMinutes)) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20),legend.position = "none")+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-11'))+
  coord_cartesian(ylim = c(0, 400))+
  ggtitle("Active people")+
  xlab("Time")+
  ylab("Minutes of activity")

plot10<-ggplot(data=activity_not_in_use)+
  geom_smooth(se=FALSE,mapping=aes(x=ActivityDate, y=LightlyActiveMinutes, color=Id)) +
  geom_smooth(mapping=aes(x=ActivityDate, y=LightlyActiveMinutes)) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20))+
  theme(legend.position = "none")+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-11'))+
  coord_cartesian(ylim = c(0, 400))+
  ggtitle("Non-active people")+
  xlab("Time")+
  ylab("Minutes of activity")

plot11<-ggplot(data=activity_in_use)+
  geom_smooth(se=FALSE,mapping=aes(x=ActivityDate, y=FairlyActiveMinutes, color=Id)) +
  geom_smooth(mapping=aes(x=ActivityDate, y=FairlyActiveMinutes)) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20),legend.position = "none")+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-11'))+
  coord_cartesian(ylim = c(0, 90))+
  ggtitle("Active people")+
  xlab("Time")+
  ylab("Minutes of activity")

plot12<-ggplot(data=activity_not_in_use)+
  geom_smooth(se=FALSE,mapping=aes(x=ActivityDate, y=FairlyActiveMinutes, color=Id)) +
  geom_smooth(mapping=aes(x=ActivityDate, y=FairlyActiveMinutes)) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20))+
  theme(legend.position = "none")+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-11'))+
  coord_cartesian(ylim = c(0, 90))+
  ggtitle("Non-active people")+
  xlab("Time")+
  ylab("Minutes of activity")

plot13<-ggplot(data=activity_in_use)+
  geom_smooth(se=FALSE,mapping=aes(x=ActivityDate, y=VeryActiveMinutes, color=Id)) +
  geom_smooth(mapping=aes(x=ActivityDate, y=VeryActiveMinutes)) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20),legend.position = "none")+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-11'))+
  coord_cartesian(ylim = c(0, 110))+
  ggtitle("Active people")+
  xlab("Time")+
  ylab("Minutes of activity")

plot14<-ggplot(data=activity_not_in_use)+
  geom_smooth(se=FALSE,mapping=aes(x=ActivityDate, y=VeryActiveMinutes, color=Id)) +
  geom_smooth(mapping=aes(x=ActivityDate, y=VeryActiveMinutes)) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 20))+
  theme(legend.position = "none")+
  xlim(as.Date('2016-04-10'), as.Date('2016-05-11'))+
  coord_cartesian(ylim = c(0, 110))+
  ggtitle("Non-active people")+
  xlab("Time")+
  ylab("Minutes of activity")

grid.arrange(plot9, plot10, ncol=2, top="Light activity")
grid.arrange(plot11, plot12, ncol=2, top="Medium activity")
grid.arrange(plot13, plot14, ncol=2, top="Heavy activity")

n_distinct(s_heartrate$Id) #will give us the amount of people
n_distinct(h_calories$Id) #will give us the amount of people
n_distinct(h_steps$Id) #will give us the amount of people
n_distinct(h_intensitie$Id) #will give us the amount of people

h_intensitie <- filter(h_intensitie, Date <= "2016-05-10")

intensitie_in_use<- filter(h_intensitie, Id %in% active_people)
intensitie_not_in_use<- filter(h_intensitie, !(Id %in% active_people))

int_acti_hour <- intensitie_in_use %>%
  group_by(Time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

int_not_acti_hour <- intensitie_not_in_use %>%
  group_by(Time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

plot15 <-ggplot(data=int_acti_hour, aes(x=Time, y=mean_total_int)) + 
  geom_histogram(stat = "identity", fill='darkblue') +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(ylim = c(0, 25))+
  labs(title="Active people")

plot16 <- ggplot(data=int_not_acti_hour, aes(x=Time, y=mean_total_int)) + 
  geom_histogram(stat = "identity", fill='darkblue') +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(ylim = c(0, 25))+
  labs(title="Non-active people")

grid.arrange(plot15, plot16, ncol=2, top="Average Total Intensity vs. Time")
random_person_1 <- sample(1:21, 3) #3 random numbers that are not repeated 
intensitie_act1<- filter(intensitie_in_use, Id==active_people[,random_person_1[1]])
intensitie_act2<- filter(intensitie_in_use, Id==active_people[,random_person_1[2]])
intensitie_act3<- filter(intensitie_in_use, Id==active_people[,random_person_1[3]])
#same for the not active people
non_active_people <- unique(activity_not_in_use$Id)
random_person_2 <- sample(1:12, 3) #3 random numbers that are not repeated 
intensitie_no_act1<- filter(intensitie_not_in_use, Id==non_active_people[random_person_2[1]])
intensitie_no_act2<- filter(intensitie_not_in_use, Id==non_active_people[random_person_2[2]])
intensitie_no_act3<- filter(intensitie_not_in_use, Id==non_active_people[random_person_2[3]])


active_1 <- intensitie_act1 %>%
  group_by(Time) %>%
  #drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

active_2 <- intensitie_act2 %>%
  group_by(Time) %>%
  #drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

active_3 <- intensitie_act3 %>%
  group_by(Time) %>%
  #drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

n_active_1 <- intensitie_no_act1 %>%
  group_by(Time) %>%
  #drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

n_active_2 <- intensitie_no_act2 %>%
  group_by(Time) %>%
  #drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

n_active_3 <- intensitie_no_act3 %>%
  group_by(Time) %>%
  #drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

plot17 <-ggplot(data=active_1, aes(x=Time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45)) +
  coord_cartesian(ylim = c(0, 25))+
  labs(title="Active person 1")

plot18 <-ggplot(data=active_2, aes(x=Time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(ylim = c(0, 25))+
  labs(title="Active person 2")

plot19 <-ggplot(data=active_3, aes(x=Time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(ylim = c(0, 25))+
  labs(title="Active person 3")

plot20 <-ggplot(data=n_active_1, aes(x=Time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(ylim = c(0, 25))+
  labs(title="Non-active person 1")

plot21 <-ggplot(data=n_active_2, aes(x=Time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(ylim = c(0, 25))+
  labs(title="Non-active person 2")

plot22 <-ggplot(data=n_active_3, aes(x=Time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme_light()+
  theme(axis.text.x = element_text(angle = 90)) +
  coord_cartesian(ylim = c(0, 25))+
  labs(title="Non-active person 3")

grid.arrange(plot17, plot18, plot19, ncol=3, top="Average Total Intensity vs. Time")
grid.arrange(plot20, plot21, plot22, ncol=3, top="Average Total Intensity vs. Time")
grid.arrange(plot7, plot8, ncol=2)
diff_int_plot
grid.arrange(plot9, plot10, ncol=2)
grid.arrange(plot11, plot12, ncol=2)
grid.arrange(plot13, plot14, ncol=2)
