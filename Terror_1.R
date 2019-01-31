#Terrorist Report

rm(list=ls())


setwd(dirname(rstudioapi::getSourceEditorContext()$path))



attacks<-read.csv('globalterrorismdb_0718dist.csv',header=TRUE)



attacks<-attacks[,-c(1,5,7,8,10,11,14,15,18,19,30,32,34,36,38,42,44,46,50,52,54,58,74,77,80,83,85,87,89,91,93,95,97,107,109,124,126,131,132,133,134,135)]



#doubterr is Y/N if doubt of proper terrorism
#alternative/ _txt have details of other groups

#RV=successs










#Afghanistan
attacks_AF<-attacks[attacks$country_txt=="Afghanistan",]

#After 2001
attacks_AF<-attacks_AF[attacks_AF$iyear>=2002,]


attackAF.fit<-glm(success~as.factor(weaptype1),data=attacks_AF,family=binomial(link=logit))
summary(attackAF.fit)




###THIS IS FOR INITIAL TIME SERIES PLOT


library('dplyr')

attacks_TS<-select(attacks_AF,iyear,imonth,iday,provstate,city,gname,success)
attacks_TS$attempts<-rep(1,nrow(attacks_TS))

for(i in 1:nrow(attacks_TS)){
  if(attacks_TS$iday[i]==0){
    attacks_TS$iday[i]<-1
  }else{
    attacks_TS$iday[i]<-attacks_TS$iday[i]
  }
}

attacks_TS$date<-as.Date(paste(attacks_TS$iyear,attacks_TS$imonth,attacks_TS$iday,sep='/'),'%Y/%m/%d')
attacks_TS$date<-substr(attacks_TS$date,1,7)
attacks.TS.gname<-aggregate(cbind(success,attempts) ~ gname+date,FUN=sum,data=attacks_TS)


#Now to create different group sets and full month lists
months.full<-seq.POSIXt(as.POSIXlt("2002-01-01"), as.POSIXlt("2017-12-01"), by="month")
months.full<-substr(months.full,1,7)

TS<-data.frame(months.full)

for(i in 1:length(unique(attacks.TS.gname$gname))){
  TS[,i+1]<-rep(0,nrow(TS))
}



colnames(TS)<-c('month',as.character(unique(attacks.TS.gname$gname)))

TS.attempts<-TS
TS.success<-TS

for(i in 1:nrow(TS.attempts)){
  for(j in 2:ncol(TS.attempts)){
    if(TS.attempts$month[i] %in% attacks.TS.gname[attacks.TS.gname$gname==colnames(TS.attempts)[j],2]){
    TS.attempts[i,j]<-attacks.TS.gname[attacks.TS.gname$date==TS.attempts$month[i] & attacks.TS.gname$gname==colnames(TS.attempts)[j],4]
    }else{
      TS.attempts[i,j]<-0
    }
  }
}

for(i in 1:nrow(TS.success)){
  for(j in 2:ncol(TS.success)){
    if(TS.attempts$month[i] %in% attacks.TS.gname[attacks.TS.gname$gname==colnames(TS.success)[j],2]){
      TS.success[i,j]<-attacks.TS.gname[attacks.TS.gname$date==TS.attempts$month[i] & attacks.TS.gname$gname==colnames(TS.success)[j],3]
    }else{
      TS.success[i,j]<-0
    }
  }
}
#Plotting Groups
#Plotting Attempt Plot

plot(TS.attempts$Taliban,type='l',axes=FALSE,main='Terrorist Attacks by Group',ylab='Attempts',xlab='Month')
axis(1,at=1:nrow(TS.attempts),labels=TS.attempts$month)
text(0,y=seq(0,max(attacks.TS.gname$attempts),5),labels=seq(0,max(attacks.TS.gname$attempts),5),pos=2)

for(i in 2:ncol(TS.attempts)){
lines(TS.attempts$month,TS.attempts[,i],type='l',col=i)
}
legend(0,180,legend=colnames(TS.attempts)[2:ncol(TS.attempts)],pch=19,col=2:ncol(TS.attempts),cex=.3)

#Plotting Success Plot

plot(TS.success$Taliban,type='l',axes=FALSE,main='Terrorist Attacks by Group',ylab='Successful Attacks',xlab='Month')
axis(1,at=1:nrow(TS.success),labels=TS.success$month)
text(0,y=seq(0,max(attacks.TS.gname$success),5),labels=seq(0,max(attacks.TS.gname$success),5),pos=2)

for(i in 2:ncol(TS.success)){
  lines(TS.success$month,TS.success[,i],type='l',col=i)
}
legend(0,160,legend=colnames(TS.success)[2:ncol(TS.success)],pch=19,col=2:ncol(TS.success),cex=.3)



###Time Series by Provincial State

attacks.TS.state<-aggregate(cbind(success,attempts) ~ provstate+date,FUN=sum,data=attacks_TS)


TS<-data.frame(months.full)

for(i in 1:length(unique(attacks.TS.state$provstate))){
  TS[,i+1]<-rep(0,nrow(TS))
}



colnames(TS)<-c('month',as.character(unique(attacks.TS.state$provstate)))

TS.attempts<-TS
TS.success<-TS

for(i in 1:nrow(TS.attempts)){
  for(j in 2:ncol(TS.attempts)){
    if(TS.attempts$month[i] %in% attacks.TS.state[attacks.TS.state$provstate==colnames(TS.attempts)[j],2]){
      TS.attempts[i,j]<-attacks.TS.state[attacks.TS.state$date==TS.attempts$month[i] & attacks.TS.state$provstate==colnames(TS.attempts)[j],4]
    }else{
      TS.attempts[i,j]<-0
    }
  }
}

for(i in 1:nrow(TS.success)){
  for(j in 2:ncol(TS.success)){
    if(TS.attempts$month[i] %in% attacks.TS.state[attacks.TS.state$provstate==colnames(TS.success)[j],2]){
      TS.success[i,j]<-attacks.TS.state[attacks.TS.state$date==TS.attempts$month[i] & attacks.TS.state$provstate==colnames(TS.success)[j],3]
    }else{
      TS.success[i,j]<-0
    }
  }
}
#Plotting Groups
#Plotting Attempt Plot

plot(TS.attempts$Nangarhar,type='l',axes=FALSE,main='Terrorist Attacks by Provincial State',ylab='Attempts',xlab='Month')
axis(1,at=1:nrow(TS.attempts),labels=TS.attempts$month)
text(0,y=seq(0,max(attacks.TS.state$attempts),1),labels=seq(0,max(attacks.TS.state$attempts),1),pos=2)

for(i in 2:ncol(TS.attempts)){
  lines(TS.attempts$month,TS.attempts[,i],type='l',col=i)
}
legend(0,180,legend=colnames(TS.attempts)[2:ncol(TS.attempts)],pch=19,col=2:ncol(TS.attempts),cex=.3)

#Plotting Success Plot

plot(TS.success$Nangarhar,type='l',axes=FALSE,main='Terrorist Attacks by Provincial State',ylab='Successful Attacks',xlab='Month')
axis(1,at=1:nrow(TS.success),labels=TS.success$month)
text(0,y=seq(0,max(attacks.TS.state$success),1),labels=seq(0,max(attacks.TS.state$success),1),pos=2)

for(i in 2:ncol(TS.success)){
  lines(TS.success$month,TS.success[,i],type='l',col=i)
}
legend(0,160,legend=colnames(TS.success)[2:ncol(TS.success)],pch=19,col=2:ncol(TS.success),cex=.3)


#Let's do total.


plot(rowSums(TS.attempts[,2:ncol(TS.attempts)]),type='l',axes=FALSE,main='Total Terrorist Attacks 2002 - 2017',ylab='Attacks',xlab='Month')
axis(1,at=1:nrow(TS.success),labels=TS.success$month)
text(0,y=seq(0,max(rowSums(TS.attempts[,2:ncol(TS.success)])),10),labels=seq(0,max(rowSums(TS.attempts[,2:ncol(TS.success)])),10),pos=2)
lines(TS.success$month,rowSums(TS.success[,2:ncol(TS.success)]),type='l',col=2)
legend(0,220,legend = c('Attempts','Successes'),col=c(1,2),pch=19)









#Variable clean. Remove Country and Year

attacks_AF<-attacks_AF[,-1]
attacks_AF<-attacks_AF[,-5]

#Remove all non-primary attacks

attacks_red<-attacks_AF[-c(9,10,12,18,19,25,26,27,28,29,30,31,32,33,34,37,38,39,
                           40,43,44,50,51,52,53,57,58,59,60,61,62,89,90)]


for(i in c(1:11,13:32,39:40,47:48)){
  attacks_red[,i]<-as.factor(attacks_red[,i])
}

attacks_red<-attacks_AF[,c(17,37,6)]



#Create data frame with prov states, total appearances, success

#1 Create col of provstate

Provstate<-as.array(unique(attacks_red$provstate))
Total<-rep(NA,36)
Success<-rep(NA,36)
prov.table<-data.frame(Provstate,Total,Success)

#2 Create col of total appearances

for(i in 1:36){
  prov.table[i,2]<-length(attacks_red[attacks_red$provstate==prov.table[i,1],12])
}


#3 Create column of success
for(i in 1:36){
  prov.table[i,3]<-sum(attacks_red[attacks_red$provstate==prov.table[i,1],12])
}

#4 Create column of success

prov.table$Prop<-round(prov.table$Success / prov.table$Total,3)


#5 Creatte column of Standard Error

prov.table$SE<- sqrt(prov.table$Prop*(1-prov.table$Prop)/prov.table$Total)


#Order and Plot
prov.table<-prov.table[order(prov.table$Prop),]

plot(1:36,prov.table$Prop,pch=19)
for(i in 1:36){
  segments(i,prov.table[i,4],i,prov.table[i,4]+prov.table[i,5])
}
for(i in 1:36){
  segments(i,prov.table[i,4],i,prov.table[i,4]-prov.table[i,5])
}

#Logistic Regression

attackred.fit<-glm(success~as.factor(provstate),data=attacks_red,family=binomial(link=logit))
summary(attackred.fit)



#Group names


#Create data frame with group names, total appearances, success

#1 Create col of provstate

Group<-as.array(unique(attacks_red$gname))
Total<-rep(NA,28)
Success<-rep(NA,28)
group.table<-data.frame(Group,Total,Success)

#2 Create col of total appearances

for(i in 1:28){
  group.table[i,2]<-length(attacks_red[attacks_red$gname==group.table[i,1],12])
}


#3 Create column of success
for(i in 1:28){
  group.table[i,3]<-sum(attacks_red[attacks_red$gname==group.table[i,1],12])
}

#4 Create column of success

group.table$Prop<-round(group.table$Success / group.table$Total,3)


#5 Creatte column of Standard Error

group.table$SE<- sqrt(group.table$Prop*(1-group.table$Prop)/group.table$Total)


#Order and Plot
group.table<-group.table[order(group.table$Prop),]

plot(1:28,group.table$Prop,pch=19)
for(i in 1:28){
  segments(i,group.table[i,4],i,group.table[i,4]+group.table[i,5])
}
for(i in 1:28){
  segments(i,group.table[i,4],i,group.table[i,4]-group.table[i,5])
}

#Logistic Regression

attackred.fit<-glm(success~as.factor(provstate),data=attacks_red,family=binomial(link=logit))
summary(attackred.fit)


#Evaluate those with more than 30 attacks. Then, compare areas.

group.red<-group.table[group.table$Total>=20,]

other.total<-sum(group.table[group.table$Total<20,2])
other.success<-sum(group.table[group.table$Total<20,3])
other<-data.frame("Other",other.total,other.success,other.success/other.total,
         sqrt((other.success/other.total)*(1-other.success/other.total)/other.total))
names(other)<-names(group.red)
group.red<-rbind(group.red,other)

attackred.fit<-glm(success~as.factor(provstate)*as.factor(gname),data=attacks_red,family=binomial(link=logit))
summary(attackred.fit)



##New thought:

#Breslow-Day and CMH test like pg 127


#Only keep group, success, and provstate

attacks_3<-attacks_red[,c(12,4,20)]

Provstate<-rep(unique(attacks_3$provstate),7)

Group<-c(as.character(rep(group.red$Group[1],36)),
         as.character(rep(group.red$Group[2],36)),
         as.character(rep(group.red$Group[3],36)),
         as.character(rep(group.red$Group[4],36)),
         as.character(rep(group.red$Group[5],36)),
         as.character(rep(group.red$Group[6],36)),
         as.character(rep(group.red$Group[7],36))
)
Success<-rep(NA,252)
Failure<-rep(NA,252)
red.table<-data.frame(Provstate,Group,Success,Failure)


for(i in 1:216){
  red.table$Success[i]<-sum(
    attacks_3[attacks_3$provstate==red.table$Provstate[i]
              & attacks_3$gname==as.character(red.table$Group[i])
              ,1])
}
for(i in 1:216){
  red.table$Failure[i]<-length(
    attacks_3[attacks_3$provstate==red.table$Provstate[i]
              & attacks_3$gname==as.character(red.table$Group[i])
              ,1])-red.table$Success[i]
}

for(i in 217:252){
  red.table$Success[i]<-sum(
    attacks_3[attacks_3$provstate==red.table$Provstate[i]
              & attacks_3$gname != as.character(red.table$Group[1])
              & attacks_3$gname != as.character(red.table$Group[2])
              & attacks_3$gname != as.character(red.table$Group[3])
              & attacks_3$gname != as.character(red.table$Group[4])
              & attacks_3$gname != as.character(red.table$Group[5])
              & attacks_3$gname != as.character(red.table$Group[6])
              ,1])
}

for(i in 217:252){
  red.table$Failure[i]<-length(
    attacks_3[attacks_3$provstate==red.table$Provstate[i]
              & attacks_3$gname != as.character(red.table$Group[1])
              & attacks_3$gname != as.character(red.table$Group[2])
              & attacks_3$gname != as.character(red.table$Group[3])
              & attacks_3$gname != as.character(red.table$Group[4])
              & attacks_3$gname != as.character(red.table$Group[5])
              & attacks_3$gname != as.character(red.table$Group[6])
              ,1])-red.table$Success[i]
}



##Map

# Get the map data of "Germany"
library(ggplot2)
library(readr)
library(dplyr)
library(purrr)

library(maps)
library(ggmap)
library(ggthemes)
library(viridis)
library(rgdal)
library(ggfortify)
library(animation)
library(gganimate)
library(gapminder)
library(car)
afghan_06 <- get_map(location = "Afghan", zoom = 6)

# Plot map and polygon on top:
ggmap(afghan_06) +
  geom_polygon(
    data = attacks_3,
    aes(x = long, y = lat, group = group),
    fill = NA, 
    col = "red") +
  coord_map()






##Logistic model

library(tidyverse)


attack_afghan<-read_csv('globalterrorismdb_0718dist.csv')


##Subset


attack_afghan<- filter(attack_afghan,country_txt=="Afghanistan",iyear>=2002)
attack_afghan<-select(attack_afghan,-country,-country_txt,-region_txt,-region,-gsubname3)


##Logistic model



attack.mod<-glm(success~.,data=attack_afghan,family=binomial(link=logit))

for(i in 1:length(colnames(attack_afghan))){
  print(
  nrow(distinct(attack_afghan[,i]))
  )
}



#Filtering down top groups

df<-attack_afghan %>% group_by(gname) %>% filter(n()>50)
ggplot(data=df)+geom_bar(mapping=aes(x=gname))

#This shows Taliban with strong foothold, unknown large






#Do seasonal analysis
TS.attempts$date<-parse_date(TS.attempts$month,"%Y-%m")
seasonal<-filter(TS.attempts,date>="2013-01-01")
seasonal<-select(seasonal,-date)
seasonal$Total<-rowSums(seasonal[2:ncol(seasonal)])
library(astsa)
library(forecast)
auto.arima(seasonal$Total)

tsplot(seasonal$Total)
plot(seasonal$Total,type='b',pch=19,col=substr(seasonal$month,6,7),axes=FALSE,ylab="Total Attemptes",xlab="Time",main="Total Attack Attempts 2013-2017")
legend(x=0,y=230,legend = 1:12,col=1:12,pch=19)
axis(2,at=seq(0,250,20))
axis(1,at=seq(1,60,12),labels = 2013:2017)

sarima(seasonal$Total,1,0,0,1,0,0,12)

sarima.for(seasonal$Total,12,1,0,0,1,0,0,12)



#This shows peaks in summer months and we can expect more attacks in the summer months.
