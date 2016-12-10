#####################Analysis of NPS by length of stay#####################
library("ggplot2")
library("gdata")

#loading the dataset
datag <- read.csv("file:///D:/Syracuse Subjects/IST687 - Applied Data Science/Project/out-hyatt_Jan2015.csv")
View(datag)
#summary(datag)

#considering only the following columns in the dataset:Likelihood_Recommend_H, Length_Stay_H
data2 <- datag[,which(names(datag) %in% c("Likelihood_Recommend_H","Length_Stay_H"))]

# Removing the NA values
data2<-na.omit(data2)
nrow(data2) #39195 entries

#Finding the maxiumum length of stay
max(data2$Length_Stay_H)


#dividing the data into subsets based on length of stay
data2s1<-data2[which(data2$Length_Stay_H<=15),]
nrow(data2s1) # 39051
data2s2<-data2[which(data2$Length_Stay_H>15),]
nrow(data2s2) # 144

#calculating the mean of likelihood to recommend for each of the subsets
mean1<-mean(data2s1$Likelihood_Recommend_H)
mean1 # 8.623
mean2<-mean(data2s2$Likelihood_Recommend_H)
mean2 # 9.006


dataMean<-c(mean1,mean2)
dataRange<-c("<=15",">15")
dataPlot<-data.frame(dataMean,dataRange)
#View(dataPlot)

g<-ggplot(dataPlot,aes(x=dataRange,y=dataMean,fill=dataRange,width=0.3))
g<-g+geom_bar(stat="identity")+ggtitle("Analysis for number of days of Visit")
g<-g+xlab("Number of days visited")+ylab("Mean(Likelihood to Recommend)")
g<-g+coord_cartesian(ylim=c(8, 10))
g

#storing data with higher length of stay into dataStay to derive insights
dataStay<-datag[which(datag$Length_Stay_H>15),]
#View(dataStay)

#Analyzing the mean for different parameters for the customers with higher stay
dataCustService<-na.omit(dataStay$Customer_SVC_H)
a<-mean(dataCustService)
dataGuestRoom<-na.omit(dataStay$Guest_Room_H)
b<-mean(dataGuestRoom)
dataStaffCared<-na.omit(dataStay$Staff_Cared_H)
c<-mean(dataStaffCared)
dataConditionHotel<-na.omit(dataStay$Condition_Hotel_H)
d<-mean(dataConditionHotel)
dataTranquility<-na.omit(dataStay$Tranquility_H)
e<-mean(dataTranquility)
dataInternetSat<-na.omit(dataStay$Internet_Sat_H)
f<-mean(dataInternetSat)

#plotting the mean for different parameters to determine the most important parameters
#first creating a dataframe containing each mean for the purpose of plotting
dataFacilityMean<-c(a,b,c,d,e,f)
dataFacility<-c("Customer Service","Guest Room","Staff Cared","Condition of Hotel","Tranquility","Internet Satisfaction")
dataPlot1<-data.frame(dataFacility,dataFacilityMean)
#View(dataPlot1)

g1<-ggplot(dataPlot1,aes(x=dataFacility,y=dataFacilityMean,fill=dataFacility,width=0.5))
g1<-g1+geom_bar(stat="identity")+ggtitle("Analysis of Customer Ratings for longer stay")
g1<-g1+xlab("Parameter")+ylab("Mean(Rating)")+theme(axis.text.x=element_text(angle=90,hjust=1))
g1<-g1+coord_cartesian(ylim=c(7, 10))
g1


#storing data with lower length of stay into dataStay1 to derive insights
dataStay1<-datag[which(datag$Length_Stay_H<=15),]
#View(dataStay)

#Analyzing the mean for different parameters for the customers with higher stay
dataCustService1<-na.omit(dataStay1$Customer_SVC_H)
a1<-mean(dataCustService1)
dataGuestRoom1<-na.omit(dataStay1$Guest_Room_H)
b1<-mean(dataGuestRoom1)
dataStaffCared1<-na.omit(dataStay1$Staff_Cared_H)
c1<-mean(dataStaffCared1)
dataConditionHotel1<-na.omit(dataStay1$Condition_Hotel_H)
d1<-mean(dataConditionHotel1)
dataTranquility1<-na.omit(dataStay1$Tranquility_H)
e1<-mean(dataTranquility1)
dataInternetSat1<-na.omit(dataStay1$Internet_Sat_H)
f1<-mean(dataInternetSat1)

#plotting the mean for different parameters to determine the most important parameters
#first creating a dataframe containing each mean for the purpose of plotting
dataFacilityMean1<-c(a1,b1,c1,d1,e1,f1)
dataFacility1<-c("Customer Service","Guest Room","Staff Cared","Condition of Hotel","Tranquility","Internet Satisfaction")
dataPlot2<-data.frame(dataFacility1,dataFacilityMean1)
#View(dataPlot2)

g2<-ggplot(dataPlot2,aes(x=dataFacility1,y=dataFacilityMean1,fill=dataFacility1,width=0.5))
g2<-g2+geom_bar(stat="identity")+ggtitle("Analysis of Customer Ratings for shorter stay")
g2<-g2+xlab("Parameter")+ylab("Mean(Rating)")+theme(axis.text.x=element_text(angle=90,hjust=1))
g2<-g2+coord_cartesian(ylim=c(7, 10))
g2
