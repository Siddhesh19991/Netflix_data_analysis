netflix<- read.csv("~/Downloads/NetflixViewingHistory.csv")

dim(netflix)

#Selecting only the titles and not the episodes

library(dplyr)
library(tidyr)
netflix<-netflix%>%separate(Title,c("Title","col1","col2"),sep=":")
netflix$col1<-NULL
netflix$col2<-NULL


#shows binged
binge<-netflix%>%select(Title,Date)%>%group_by(Title,Date)%>%summarise(total=n())%>%arrange(desc(total))
binge<-as.data.frame(binge)

#According to sources a binge is when you watch 2-6 episodes in one sitting 
#So for this analysis i will take 6 episodes

binge<-subset(binge,total>=6)

library(ggplot2)

binge<-binge%>%top_n(10)
ggplot(binge,aes(total,Title))+geom_bar(stat = "identity")

#so the most binged show was "big mouth" dated on 6/10/2018
       

#most viewed show
view<-netflix%>%select(Title,Date)%>%group_by(Title)%>%summarise(total=n())%>%arrange(desc(total))
view<-as.data.frame(view)


view<-view%>%top_n(10)
ggplot(view,aes(total,Title))+geom_bar(stat = "identity")


#most viewed show is "Friends"


#time analysis of netflix data

library(lubridate)

netflix$Date<-dmy(netflix$Date)

netflix$day<-weekdays(netflix$Date)


day_analysis<-netflix%>%count(Date)%>%arrange(desc(n))
#2019-03-09 14 and 2020-02-15 14 :the two days where i binged the most shows and movies (14)


ggplot(day_analysis,aes(Date,n))+geom_bar(stat = "identity")

#we see that the amount of netflix shows/movies consumed has increased

day_analysis$year<-year(day_analysis$Date)
day_analysis$month<-month(day_analysis$Date)
day_analysis$day<-weekdays(day_analysis$Date)
day_analysis$week<-ceiling(day(day_analysis$Date)/7)
a<-day_analysis$month
day_analysis$month<-ifelse(a==1,"jan",ifelse(a==2,"feb",ifelse(a==3,"mar",ifelse(a==4,"apr",ifelse(a==5,"may",ifelse(a==6,"jun",ifelse(a==7,"jul",ifelse(a==8,"aug",ifelse(a==9,"sep",ifelse(a==10,"oct",ifelse(a==11,"nov","dec")))))))))))

ggplot(day_analysis,aes(week,day))+geom_tile(aes(fill=n))+facet_grid(year~month)+scale_fill_gradient(low = "#FFD000", high = "#FF1919")
#daily consumption through all the years 


#which day of the week is my consumption the highest

ggplot(day_analysis,aes(day,n))+coord_polar()+ geom_col(fill = "#5b59d6")

#thursdays is the most

#which month of the year is my consumption the highest

ggplot(day_analysis,aes(month,n))+coord_polar()+ geom_col(fill = "#5b59d6")

#june is the highest


#which month of all the years is my consumption the highest

day_analysis$month_year<-paste(day_analysis$month,day_analysis$year,sep = " ")
ggplot(day_analysis,aes(month_year,n))+coord_polar()+ geom_col(fill = "#5b59d6")

#march 2020 is the highest

#2020 comparision with other years(from 2017)
#consisdering only the first 6 months

day_analysis<-subset(day_analysis,year!=2016)
day_analysis$m<-month(day_analysis$Date)
day_analysis<-subset(day_analysis,m<=6)

ggplot(day_analysis,aes(n,month))+geom_bar(stat = "identity")+facet_grid(~year)

#we see that their is very high consumption in the year 2020 compared to the other years
#one of the reasons could be the spread of the COVID-19 virus which has forced people to stay at home 
#and with a lot of time in our hands , the consumption of netflix has dramatically increased
