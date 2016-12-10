## Comparing facilites of Top 3 NPS States of US vs Bottom 3 NPS State of US

# Installing and Loading the required packages
install.packages("ggplot2")
library("ggplot2")

# loading the csv file
Jan.2015 <- read.csv("file:///D:/Syracuse Subjects/IST687 - Applied Data Science/Project/out-hyatt_Jan2015.csv")
View(Jan.2015) # 1.7M entries

# subset likelihood to recommend
## Subset of only promoters
myPromoters<-subset(Jan.2015,Likelihood_Recommend_H>=9)
nrow(myPromoters) # 26 K
## Subset of only detracter
myDetractors<-subset(Jan.2015,Likelihood_Recommend_H<=6)
nrow(myDetractors) # 4762 entries
# Subset of only Passive Entries
myPassive<-subset(Jan.2015,Likelihood_Recommend_H<=8&Likelihood_Recommend_H>=7)
nrow(myPassive) # 7715 entries

##############################################################################################################
####################################### subset of top3 #######################################################

myTop3P <- subset(myPromoters, STATE_R == "MT" | STATE_R == "NE" | STATE_R == "TN")
nrow(myTop3P)
View(myTop3P)

#guest
PromoterGuest <- tapply(myTop3P$Guest_Room_H, myTop3P$Guest_Room_H, length)
# Promoter Guest Percentage
PGP <- (PromoterGuest[[4]]+PromoterGuest[[5]])/sum(PromoterGuest)*100
# internet
PromoterInternet <- tapply(myTop3P$Internet_Sat_H, myTop3P$Internet_Sat_H, length)
# Promoter Internet Percentage
PIP <- (PromoterInternet[[8]]+PromoterInternet[[9]])/sum(PromoterInternet)*100
# tranquality
PromoterTranquality <- tapply(myTop3P$Tranquility_H, myTop3P$Tranquility_H, length)
# Promoter Tranquality Percentage
PTP <- (PromoterTranquality[[6]]+PromoterTranquality[[7]])/sum(PromoterTranquality)*100
# condition of hotel
PromoterCH <- tapply(myTop3P$Condition_Hotel_H, myTop3P$Condition_Hotel_H, length)
# Promoter Condition of Hotel Percentage
PCHP <- (PromoterCH[[4]]+PromoterCH[[5]])/sum(PromoterCH)*100
#customer service
PromoterCS <- tapply(myTop3P$Customer_SVC_H, myTop3P$Customer_SVC_H, length)
# Promoter Customer Service Percentage
PCSP <- (PromoterCS[[5]]+PromoterCS[[6]])/sum(PromoterCS)*100
# staff care
PromoterStaffCare <- tapply(myTop3P$Staff_Cared_H, myTop3P$Staff_Cared_H, length)
# Promoter Staff Care Percentage
PSCP <- (PromoterStaffCare[[7]]+PromoterStaffCare[[8]])/sum(PromoterStaffCare)*100

myTop3D <- subset(myDetractors, STATE_R == "MT" | STATE_R == "NE" | STATE_R == "TN")
nrow(myTop3D)

#################################################################################################################
###################################### subset of Bottom 3 #######################################################

myBottom3P <- subset(myPromoters, STATE_R == "ND" | STATE_R == "RI" | STATE_R == "WY")
nrow(myBottom3P)

myBottom3D <- subset(myDetractors, STATE_R == "ND" | STATE_R == "RI" | STATE_R == "WY")
nrow(myBottom3D)

# guest
DetractorGuest <- tapply(myBottom3D$Guest_Room_H, myBottom3D$Guest_Room_H, length)
# Detractor Guest Percentage
DGP <- DetractorGuest[[9]]/sum(DetractorGuest)*100
# internet
DetractorInternet <- tapply(myBottom3D$Internet_Sat_H, myBottom3D$Internet_Sat_H, length)
# Detractor Internet Percentage
DIP <- (DetractorInternet[[3]]+DetractorInternet[[4]])/sum(DetractorInternet)*100
# tranquality
DetractorTranquality <- tapply(myBottom3D$Tranquility_H, myBottom3D$Tranquility_H, length)
# Detractor Tranquality Percentage
DTP <- (DetractorTranquality[[6]]+DetractorTranquality[[7]])/sum(DetractorTranquality)*100
# condition of hotel
DetractorCH <- tapply(myBottom3D$Condition_Hotel_H, myBottom3D$Condition_Hotel_H, length)
# Detractor Condition of Hotel Percentage
DCHP <- (DetractorCH[[9]]+DetractorCH[[10]])/sum(DetractorCH)*100
# customer service
DetractorCS <- tapply(myBottom3D$Customer_SVC_H, myBottom3D$Customer_SVC_H, length)
# Detractor Customer Service Percentage
DCSP <- (DetractorCS[[8]]+DetractorCS[[9]])/sum(DetractorCS)*100
# staff care
DetractorStaffCare <- tapply(myBottom3D$Staff_Cared_H, myBottom3D$Staff_Cared_H, length)
# Detractor Staff Care Percentage
DSCP <- (DetractorStaffCare[[6]]+DetractorStaffCare[[7]])/sum(DetractorStaffCare)*100

##### Table #########################
### Visualizing the results

# creating vector to store values corresponding to the facilities
column1 <- c("Top3", "Bottom3","Top3", "Bottom3","Top3", "Bottom3","Top3", "Bottom3","Top3", "Bottom3","Top3", "Bottom3")
column2 <- c("GuestRoom", "GuestRoom", "Internet","Internet", "Tranquility","Tranquility","Condition_H", 
             "Condition_H", "CustomerService","CustomerService", "StaffService", "StaffService")
column3 <- c(PGP, DGP, PIP, DIP, PTP, DTP, PCHP, DCHP, PCSP, DCSP, PSCP, DSCP)

# creating the final dataframe for visualization
data.frame.df <- data.frame(column1,column2,column3)
colnames(data.frame.df) <- c("Category","SubCategory","Value")
View(data.frame.df) # view the dataframe

# plot for top 3 NPS states and bottom 3 NPS states
ggplot(data.frame.df, aes(factor(SubCategory), Value, fill = Category, width = 0.5)) +
  geom_bar(stat = "identity", position = "dodge") + 
   scale_fill_manual(values = c("tomato1","lightseagreen")) +
    xlab("Facilities") + ylab("Percentage of Promoters") + 
     ggtitle("Comparison of Top 3 States (Promoters) and Bottom 3 States (Detractors)") +
      theme(axis.text.x=element_text(size=15), axis.title=element_text(size=14,face = "bold"))

############################################ End of Program ####################################################
################################################################################################################