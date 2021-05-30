##PREPARE----
#-----------
#Load packages----
library(tidyverse)
library(here)
library(skimr)
library(readxl)
library(janitor)
library(magrittr)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(RODBC)
library(odbc)
library(ggplot2)
library(scales)
library(flexdashboard)

#Connect to database ----

data_connect <- dbConnect(odbc::odbc(), "R_COnnections")

# Read and sort data----

Trip_Summary<-dbGetQuery(data_connect,'Select Ride_Period, Period AS Ride_Period_Description, Rider_Type, NumberOfTrips, HoursRidden, AverageHoursPerTrip FROM Trips_Summary Order by Ride_Period')
Grand_Summary<-dbGetQuery(data_connect,'Select * from Grand_Summary ORDER BY Rider_Type DESC')

#-------------------------

##VISUALIZE------


#Plot1:Total Trips Taken-----
Grand_Summary%>%ggplot(aes(x = Rider_Type, y = NumberOfTrips, fill=Rider_Type)) + 
  geom_bar(width =0.75, stat = "identity", orientation = "Horizaontal", show.legend = FALSE)+
  ggtitle("Number of Trips by Members and Casual Riders")+
  labs(x=element_blank(), y="Trips", subtitle = "From May 2020 To April 2021")+
  theme(plot.title = element_text(face="bold", size=15, hjust=0.5)) +
  theme(axis.title = element_text(face="bold", size=11))+
  theme(plot.subtitle = element_text(face="bold", size=11, hjust=0.5))+
  theme(text = element_text(size=13, face="bold"))+
  theme(axis.text.y=element_blank())+
  geom_text(aes(label = format(NumberOfTrips, big.mark=",")), hjust = 0.5, vjust=5,fontface="bold", colour = "black")
  

#Plot2:Total Hours Ridden-----------
Grand_Summary%>%ggplot(aes(x = Rider_Type, y = HoursRidden, fill=Rider_Type)) + 
  geom_bar(width =0.75,stat = "identity", orientation = "Horizaontal", show.legend = FALSE)+
  ggtitle("Hours Ridden by Members and Casual Riders")+
  labs(x=element_blank(), y="Hours", subtitle = "May 2020 to April 2021")+
  theme(plot.title = element_text(face="bold", size=15, hjust=0.5)) +
  theme(axis.title = element_text(face="bold", size=11))+
  theme(plot.subtitle = element_text(face="bold", size=11, hjust=0.5))+
  theme(text = element_text(size=13, face="bold"))+
  theme(axis.text.y=element_blank())+
  geom_text(aes(label = format(HoursRidden, big.mark=",")), hjust = 0.5, vjust=5, fontface="bold", colour = "black")
  


#Plot3:Overall Average Hours Ridden Per Trip-----
Grand_Summary%>%ggplot(aes(x = Rider_Type, y = AverageHoursPerTrip, fill=Rider_Type)) + 
  geom_bar(width =0.75,stat = "identity", orientation = "Horizaontal", show.legend = FALSE)+
  ggtitle("Overall Average Hours Per Trip")+
  labs(x=element_blank(), y="Average Hours/Trip", subtitle = "May 2020 to April 2021")+
  theme(plot.title = element_text(face="bold", size=15, hjust=0.5)) +
  theme(axis.title = element_text(face="bold", size=11))+
  theme(plot.subtitle = element_text(face="bold", size=11, hjust=0.5))+
  theme(text = element_text(size=13, face="bold"))+
  geom_text(aes(label = AverageHoursPerTrip), hjust = 0.5, vjust=5, fontface="bold", colour = "black")


#Plot4: Trips Taken Monthly-----
Trip_Summary%>%ggplot(aes(x=Ride_Period, y=NumberOfTrips, group = Rider_Type, colour = Rider_Type))   + 
  geom_line(lwd=1.5)+
  geom_point(size=3)+
  ggtitle("Number of Trips by Members and Casual Riders")+
  labs(x=element_blank(), y="Trips", subtitle = "From May 2020 To April 2021", col="Rider Type")+
  theme(plot.title = element_text(face="bold", size=15, hjust=0.5)) +
  theme(axis.title = element_text(face="bold", size=11))+
  theme(plot.subtitle = element_text(face="bold", size=11, hjust=0.5))+
  scale_y_continuous(labels = comma)+
  theme(axis.text.x=element_text(size=10, angle=45, face="bold", hjust=0.95))+
  theme(axis.text.y=element_text(size=10, face="bold"))+
  theme(legend.title = element_text(colour="Black", size=11, face="bold"))+
  theme(legend.text = element_text(colour="Black", size=10,face="bold"))+
  theme(legend.position=c(.9,.9))+
  guides(fill=guide_legend(title="Rider Type"))+
  scale_x_discrete(breaks=c("2020-05","2020-06","2020-07","2020-08","2020-09","2020-10","2020-11","2020-12","2021-01","2021-02","2021-03","2021-04"), labels=c(" 2020   MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","2021   JAN","FEB","MAR","APR"))



#Plot5: Hours Ridden Monthly------
Trip_Summary%>%ggplot(aes(x=Ride_Period, y=HoursRidden, group = Rider_Type, colour = Rider_Type)) + 
  geom_line(lwd=1.5)+
  geom_point(size=3)+
  ggtitle("Hours Ridden by Members and Casual Riders")+
  labs(x=element_blank(), y="Hours", subtitle = "May 2020 to April 2021", col="Rider Type")+
  theme(plot.title = element_text(face="bold", size=15, hjust=0.5)) +
  theme(axis.title = element_text(face="bold", size=11))+
  theme(plot.subtitle = element_text(face="bold", size=11, hjust=0.5))+
  scale_y_continuous(labels = comma)+
  theme(axis.text.x=element_text(size=10, angle=45, face="bold", hjust = 0.95))+
  theme(axis.text.y=element_text(size=10, face="bold"))+
  theme(legend.title = element_text(colour="Black", size=11, face="bold"))+
  theme(legend.text = element_text(colour="Black", size=10,face="bold"))+
  theme(legend.position=c(.9,.9))+
  scale_x_discrete(breaks=c("2020-05","2020-06","2020-07","2020-08","2020-09","2020-10","2020-11","2020-12","2021-01","2021-02","2021-03","2021-04"), labels=c(" 2020   MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","2021   JAN","FEB","MAR","APR"))
  


#Plot6: Monthly Average Hours Per Trip ------
Trip_Summary %>% ggplot(aes(x=Ride_Period, y=AverageHoursPerTrip, group = Rider_Type, colour = Rider_Type)) +
  geom_line(lwd=1.5)+
  geom_point(size=3)+
  ggtitle("Average Hours Per Trip")+
  labs(x=element_blank(), y="Hours", subtitle = "May 2020 to April 2021", col="Rider Type")+
  theme(plot.title = element_text(face="bold", size=15, hjust=0.5)) +
  theme(axis.title = element_text(face="bold", size=11))+
  theme(plot.subtitle = element_text(face="bold", size=11, hjust=0.5))+
  theme(axis.text.x=element_text(size=10, angle=45, face="bold", hjust = 0.95))+
  theme(axis.text.y=element_text(size=10, face="bold"))+
  theme(legend.title = element_text(colour="Black", size=11, face="bold"))+
  theme(legend.text = element_text(colour="Black", size=10,face="bold"))+
  theme(legend.position=c(.9,.9))+
  scale_x_discrete(breaks=c("2020-05","2020-06","2020-07","2020-08","2020-09","2020-10","2020-11","2020-12","2021-01","2021-02","2021-03","2021-04"), labels=c(" 2020   MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","2021   JAN","FEB","MAR","APR"))
  