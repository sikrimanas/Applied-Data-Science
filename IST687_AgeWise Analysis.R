myData_Jan2015 <- read.csv("file:///D:/Syracuse Subjects/IST687 - Applied Data Science/Project/out-hyatt_Jan2015.csv")
View(myData_Jan2015)
nrow(myData_Jan2015)

# subset of promoter
pro<-subset(myData_Jan2015,Likelihood_Recommend_H>=9)
# subset of detractors
det<-subset(myData_Jan2015,Likelihood_Recommend_H<=6)

# age wise customer satisfaction for promoters
q1<-qplot(factor(Age_Range_H), data=na.omit(pro), geom="bar", fill=factor(Customer_SVC_H), main = "promoter")
q1
# age wise customer satisfaction for detractor
q2<-qplot(factor(Age_Range_H), data=na.omit(det), geom="bar", fill=factor(Customer_SVC_H), main = "detractor")
q2