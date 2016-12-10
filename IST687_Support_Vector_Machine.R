#SVM Modelling to Predict NPS_Type

# using the original data set and updating it into the new variable
SVM.Data <- myData_Jan2015
View(SVM.Data)

# Promoter Data Sets
SVM.Data.Promoter <- SVM.Data[SVM.Data$NPS_Type == "Promoter",]
View(SVM.Data.Promoter) # around 26k promoters entries
# Detractor Data Sets
SVM.Data.Detractor <- SVM.Data[SVM.Data$NPS_Type == "Detractor",]
View(SVM.Data.Detractor) # around 4k detractors entries
# Passive Data Sets
SVM.Data.Passive <- SVM.Data[SVM.Data$NPS_Type == "Passive",]
View(SVM.Data.Passive) # around 7K passive entries

# combining the three data sets into one
SVM.Data.Final <- rbind(SVM.Data.Promoter, SVM.Data.Detractor, SVM.Data.Passive)
View(SVM.Data.Final) # 39K entries

tabulate(SVM.Data.Final$GROUPS_VS_FIT_R)
unique(SVM.Data.Final$GROUPS_VS_FIT_R)

# to analyse the SVM model techniques
# we need to divide the data set into train and test data sets
########################################################################################################################
########################################################################################################################
# cleaning the data set
# checking for the column gp_tier
SVM.Data.Final$GP_Tier <- tolower(SVM.Data.Final$GP_Tier)
SVM.Data.Final$GP_Tier <- factor(SVM.Data.Final$GP_Tier)

# accessing only those rows that have value
SVM.Data.GP <- SVM.Data.Final[SVM.Data.Final$GP_Tier != "",]
View(SVM.Data.GP)


library("kernlab")
# View the dimensions of the NY State dataset
dim(SVM.Data.GP)
# Create a varaible to store index
randIndex <- sample(1:dim(SVM.Data.GP)[1])
#summary
summary(randIndex)
# verify the data by checking the length of the index variable
length(randIndex)

# Creating two datasets
# Identify the cut point to differentiate the dataset
myCutPoint2_3 <- floor(2*dim(SVM.Data.GP)[1]/3)
myCutPoint2_3

# training data
SVM.Train.GP <- SVM.Data.GP[randIndex[1:myCutPoint2_3],]
View(SVM.Train.GP)

# Test data
SVM.Test.GP <- SVM.Data.GP[randIndex[(myCutPoint2_3+1):dim(SVM.Data.GP)[1]],]
View(SVM.Test.GP)

# 1. Testing with GP.Tier : gold or platinum
SVM.Outout.1 <- ksvm(NPS_Type~GP_Tier, data = SVM.Train.GP, kernel = "rbfdot", kpar = "automatic",
                  C=5, cross = 10, prob.model = TRUE)
SVM.Outout.1

###############################################################################################################
############################################# Brand of Hotel###################################################

## now testing the model with 
unique(SVM.Data.Final$Brand_PL)

# accessing only those rows that have value
SVM.Data.Brand <- SVM.Data.Final[SVM.Data.Final$Brand_PL != "",]
View(SVM.Data.Brand)

# View the dimensions of the NY State dataset
dim(SVM.Data.Brand)
# Create a varaible to store index
randIndex <- sample(1:dim(SVM.Data.Brand)[1])
#summary
summary(randIndex)
# verify the data by checking the length of the index variable
length(randIndex)

# Creating two datasets
# Identify the cut point to differentiate the dataset
myCutPoint2_3 <- floor(2*dim(SVM.Data.Brand)[1]/3)
myCutPoint2_3

# training data
SVM.Train.Brand <- SVM.Data.Brand[randIndex[1:myCutPoint2_3],]
View(SVM.Train.Brand)

# Test data
SVM.Test.Brand <- SVM.Data.Brand[randIndex[(myCutPoint2_3+1):dim(SVM.Data.Brand)[1]],]
View(SVM.Test.Brand)

# 1. Testing with GP.Tier : gold or platinum
SVM.Output.2 <- ksvm(NPS_Type~Brand_PL, data = SVM.Train.Brand, kernel = "rbfdot", kpar = "automatic",
                     C=5, cross = 3, prob.model = TRUE) # no better output

########################################################################################################################
########################################################################################################################
###########################################Channel Category#############################################################

unique(SVM.Data.Final$Channel_Category)

# accessing only those rows that have value
SVM.Data.Channel <- SVM.Data.Final[SVM.Data.Final$Channel_Category != "",]
View(SVM.Data.Channel)

# View the dimensions of the NY State dataset
dim(SVM.Data.Channel)
# Create a varaible to store index
randIndex <- sample(1:dim(SVM.Data.Channel)[1])
#summary
summary(randIndex)
# verify the data by checking the length of the index variable
length(randIndex)

# Creating two datasets
# Identify the cut point to differentiate the dataset
myCutPoint2_3 <- floor(2*dim(SVM.Data.Channel)[1]/3)
myCutPoint2_3

# training data
SVM.Train.Channel <- SVM.Data.Channel[randIndex[1:myCutPoint2_3],]
View(SVM.Train.Brand)

# Test data
SVM.Test.Channel <- SVM.Data.Channel[randIndex[(myCutPoint2_3+1):dim(SVM.Data.Channel)[1]],]
View(SVM.Test.Brand)

# 1. Testing with GP.Tier : gold or platinum
SVM.Model.Channel <- ksvm(NPS_Type~Channel_Category, data = SVM.Train.Channel, kernel = "rbfdot", kpar = "automatic",
                     C=5, cross = 10, prob.model = TRUE) # no better output

# Important #
########################################################################################################################
########################################################################################################################
##############################################GROUP vs FIT##############################################################

unique(SVM.Data.Final$GROUPS_VS_FIT_R)

# accessing only those rows that have value
SVM.Data.Groups <- SVM.Data.Final[SVM.Data.Final$GROUPS_VS_FIT_R != "",]
SVM.Data.Groups$GROUPS_VS_FIT_R <- na.omit(SVM.Data.Groups$GROUPS_VS_FIT_R)
View(SVM.Data.Groups)
str(SVM.Data.Groups)
SVM.Data.Groups$NPS_Type <- gsub("Promoter", "Yes", SVM.Data.Groups$NPS_Type)
SVM.Data.Groups$NPS_Type <- gsub("Detractor", "No", SVM.Data.Groups$NPS_Type)
SVM.Data.Groups$NPS_Type <- factor(SVM.Data.Groups$NPS_Type)

# View the dimensions of the NY State dataset
dim(SVM.Data.Channel)
# Create a varaible to store index
randIndex <- sample(1:dim(SVM.Data.Groups)[1])
#summary
summary(randIndex)
# verify the data by checking the length of the index variable
length(randIndex)

# Creating two datasets
# Identify the cut point to differentiate the dataset
myCutPoint2_3 <- floor(2*dim(SVM.Data.Groups)[1]/3)
myCutPoint2_3

# training data
SVM.Train.Groups <- SVM.Data.Groups[randIndex[1:myCutPoint2_3],]
View(SVM.Train.Groups)

# Test data
SVM.Test.Groups <- SVM.Data.Groups[randIndex[(myCutPoint2_3+1):dim(SVM.Data.Groups)[1]],]
View(SVM.Test.Groups)

# 1. Testing with GP.Tier : gold or platinum
SVM.Model.Groups <- ksvm(NPS_Type~GROUPS_VS_FIT_R, data = SVM.Train.Groups, kernel = "rbfdot", kpar = "automatic",
                          C=50, cross = 3, prob.model = TRUE)

# predicting the model
SVM.Predict <- predict(SVM.Model.Groups, SVM.Test.Groups)
SVM.Test.Groups$Predict <- SVM.Predict

tabulate(SVM.Test.Groups$Predict)

# confusion matrix
Confusion.Matrix.groups <- confusionMatrix(SVM.Test.Groups$NPS_Type, SVM.Test.Groups$Predict)
#Confusion Matrix and Statistics
#
#Reference
#Prediction   No  Yes
#No     0 1604
#Yes    0 8890
#
#Accuracy : 0.8472

# Important #
########################################################################################################################
########################################################################################################################
##############################################Member Status#############################################################

unique(SVM.Data.Final$MEMBER_STATUS_R)
tabulate(SVM.Data.Final$MEMBER_STATUS_R)

SVM.Data.Member <- SVM.Data.Final[SVM.Data.Final$MEMBER_STATUS_R != "",]
nrow(SVM.Data.Member)
View(SVM.Data.Member)
str(SVM.Data.Member)

SVM.Data.Member$NPS_Type <- gsub("Promoter", "Yes", SVM.Data.Member$NPS_Type)
SVM.Data.Member$NPS_Type <- gsub("Detractor", "No", SVM.Data.Member$NPS_Type)
SVM.Data.Member$NPS_Type <- factor(SVM.Data.Member$NPS_Type)

# View the dimensions of the NY State dataset
dim(SVM.Data.Channel)
# Create a varaible to store index
randIndex <- sample(1:dim(SVM.Data.Member)[1])
#summary
summary(randIndex)
# verify the data by checking the length of the index variable
length(randIndex)

# Creating two datasets
# Identify the cut point to differentiate the dataset
myCutPoint2_3 <- floor(2*dim(SVM.Data.Member)[1]/3)
myCutPoint2_3

# training data
SVM.Train.Member <- SVM.Data.Member[randIndex[1:myCutPoint2_3],]
View(SVM.Train.Member)

# Test data
SVM.Test.Member <- SVM.Data.Member[randIndex[(myCutPoint2_3+1):dim(SVM.Data.Member)[1]],]
View(SVM.Test.Member)

# 1. Testing with GP.Tier : gold or platinum
SVM.Model.Members <- ksvm(NPS_Type~MEMBER_STATUS_R+GROUPS_VS_FIT_R, data = SVM.Train.Member, kernel = "rbfdot", kpar = "automatic",
                         C=50, cross = 3, prob.model = TRUE)

# predicting the model
Predict.Model <- predict(SVM.Model.Members, SVM.Test.Member)
SVM.Test.Member$Predict <- Predict.Model

# Irrelevant #
########################################################################################################################
########################################################################################################################
# trying to go with either promoter or non promoters

SVM.Data.Final$NPS_Type <- gsub("Promoter", "Yes", SVM.Data.Final$NPS_Type)
SVM.Data.Final$NPS_Type <- gsub("Detractor", "No", SVM.Data.Final$NPS_Type)
SVM.Data.Final$NPS_Type <- gsub("Passive", "No", SVM.Data.Final$NPS_Type)
SVM.Data.Final$NPS_Type <- as.factor(SVM.Data.Final$NPS_Type)
View(SVM.Data.Final)
tabulate(SVM.Data.Final$NPS_Type)

SVM.Data.Member <- SVM.Data.Final[SVM.Data.Final$MEMBER_STATUS_R != "",]
nrow(SVM.Data.Member)
tabulate(SVM.Data.Member$NPS_Type)

# View the dimensions of the NY State dataset
dim(SVM.Data.Member)
# Create a varaible to store index
randIndex <- sample(1:dim(SVM.Data.Member)[1])
#summary
summary(randIndex)
# verify the data by checking the length of the index variable
length(randIndex)

# Creating two datasets
# Identify the cut point to differentiate the dataset
myCutPoint2_3 <- floor(2*dim(SVM.Data.Member)[1]/3)
myCutPoint2_3

########################################################################################################################
########################################################################################################################

# training data
SVM.Train.Member <- SVM.Data.Member[randIndex[1:myCutPoint2_3],]
View(SVM.Train.Member)

# Test data
SVM.Test.Member <- SVM.Data.Member[randIndex[(myCutPoint2_3+1):dim(SVM.Data.Member)[1]],]
View(SVM.Test.Member)

# 1. Testing with GP.Tier : gold or platinum
SVM.Model.Members <- ksvm(NPS_Type~MEMBER_STATUS_R, data = SVM.Train.Member, kernel = "rbfdot", kpar = "automatic",
                          C=50, cross = 3, prob.model = TRUE)

# predicting the model
Predict.Model <- predict(SVM.Model.Members, SVM.Test.Member)
SVM.Test.Member$Predict <- Predict.Model
View(SVM.Test.Member)

tabulate(SVM.Test.Member$Predict)

#Visualizations#
########################################################################################################################
########################################################################################################################

View(SVM.Data.Final)

SVM.Data.Final$NPS_Type <- gsub("Promoter", "Yes", SVM.Data.Final$NPS_Type)
SVM.Data.Final$NPS_Type <- gsub("Detractor", "No", SVM.Data.Final$NPS_Type)
SVM.Data.Final$NPS_Type <- gsub("Passive", "No", SVM.Data.Final$NPS_Type)
SVM.Data.Final$NPS_Type <- as.factor(SVM.Data.Final$NPS_Type)

# rows having promoters
Group <- SVM.Data.Final[SVM.Data.Final$NPS_Type == "Yes",]
View(Group)

# rows with non promoters
Group1 <- SVM.Data.Final[SVM.Data.Final$NPS_Type == "No",]
View(Group1)

a <- unique(Group$GROUPS_VS_FIT_R)
b <- tabulate(Group$GROUPS_VS_FIT_R)

Group_Type <- c("FIT", "Group")
No_of_Promoter <- c(b[2], b[3])

Promoter.Stats <- data.frame(Group_Type,No_of_Promoter)

### For other group

c <- unique(Group1$GROUPS_VS_FIT_R)
d <- tabulate(Group1$GROUPS_VS_FIT_R)

No_of_NonPromoters <- c(d[2], d[3])

Detractor.Stats <- data.frame(Group_Type, No_of_NonPromoters)

View(Promoter.Stats)
View(Detractor.Stats)

# column bind
Data.Stats <- cbind(Promoter.Stats,Detractor.Stats)
Data.Stats <- Data.Stats[,-3]
View(Data.Stats)

Data.Stats$Total <- Data.Stats$No_of_Promoter + Data.Stats$No_of_NonPromoters

Data.Stats$PercentagePromoter <- (Data.Stats$No_of_Promoter/Data.Stats$Total)*100
Data.Stats$PercentageNonPromoter <- (Data.Stats$No_of_NonPromoters/Data.Stats$Total)*100

#barchart

ggplot(Data.Stats, aes(x = Group_Type, y = No_of_Promoter, group = Group_Type, fill = PercentagePromoter)) +
  geom_area(position = "fill")

install.packages("scales")
library(scales)
library(ggplot2)
g1 <- ggplot(Data.Stats,aes(x = Group_Type, y = No_of_Promoter)) + 
  geom_bar(stat = "identity", colour = "Blue", fill = "LightBlue") +
   xlab("Group Type") + ylab("Number of Promoters") +
    ggtitle("Promoters vs Groups")

g2 <- ggplot(Data.Stats,aes(x = Group_Type, y = No_of_NonPromoters)) + 
  geom_bar(stat = "identity", colour = "Red", fill = "coral2") +
  xlab("Group Type") + ylab("Number of Non Promoters") +
  ggtitle("Non Promoters vs Groups")

library(gridExtra)
grid.arrange(g1,g2, nrow = 1)

########################################################################################################################
########################################################################################################################
########################################################## Location ###############################################


SVM.Data.Final$NPS_Type <- gsub("Promoter", "Yes", SVM.Data.Final$NPS_Type)
SVM.Data.Final$NPS_Type <- gsub("Detractor", "No", SVM.Data.Final$NPS_Type)
SVM.Data.Final$NPS_Type <- factor(SVM.Data.Final$NPS_Type)

unique(SVM.Data.Final$Location_PL)
tabulate(SVM.Data.Final$Location_PL)

View(SVM.Data.Final)

SVM.Data.Location <- SVM.Data.Final[SVM.Data.Final$Location_PL != "",]
nrow(SVM.Data.Location)
View(SVM.Data.Location)
str(SVM.Data.Location)

# View the dimensions of the NY State dataset
dim(SVM.Data.Location)
# Create a varaible to store index
randIndex <- sample(1:dim(SVM.Data.Location)[1])
#summary
summary(randIndex)
# verify the data by checking the length of the index variable
length(randIndex)

# Creating two datasets
# Identify the cut point to differentiate the dataset
myCutPoint2_3 <- floor(2*dim(SVM.Data.Location)[1]/3)
myCutPoint2_3

# training data
SVM.Train.Location <- SVM.Data.Location[randIndex[1:myCutPoint2_3],]
View(SVM.Train.Location)

# Test data
SVM.Test.Location <- SVM.Data.Location[randIndex[(myCutPoint2_3+1):dim(SVM.Data.Location)[1]],]
View(SVM.Test.Location)

# 1. Testing with GP.Tier : gold or platinum
SVM.Model.Location <- ksvm(NPS_Type~Location_PL, data = SVM.Train.Location, kernel = "rbfdot", kpar = "automatic",
                          C=5, cross = 3, prob.model = TRUE)

# predicting the model
Predict.Location <- predict(SVM.Model.Location, SVM.Test.Location)
SVM.Test.Location$Predict <- Predict.Location

tabulate(SVM.Test.Location$Predict)

#####################################################################################################################
################################################## Taking Individual COlumn #########################################

## USA
## Business

SVM.Data.USA <- SVM.Data.Final[SVM.Data.Final$COUNTRY_CODE_R == "UNITED STATES",]
nrow(SVM.Data.USA) # 27k entries
View(SVM.Data.USA) 

SVM.USA.L <- SVM.Data.USA[SVM.Data.USA$Location_PL != "",]
View(SVM.USA.Business) #22k entries
nrow(SVM.USA.Business) # 22064

## Business Center
## Conference Center
## Limo Service
## Valet Parking

# creating data frame with only that column
a <- data.frame(SVM.USA.L$Location_PL, SVM.USA.L$NPS_Type)
colnames(a) <- c("Location", "NPS_Type")
View(a)

# View the dimensions of the NY State dataset
dim(a)
# Create a varaible to store index
randIndex <- sample(1:dim(a)[1])
#summary
summary(randIndex)
# verify the data by checking the length of the index variable
length(randIndex)

# Creating two datasets
# Identify the cut point to differentiate the dataset
myCutPoint2_3 <- floor(2*dim(a)[1]/3)
myCutPoint2_3

# training data
SVM.Train.A <- a[randIndex[1:myCutPoint2_3],]
View(SVM.Train.A)
str(SVM.Train.USA.Business)
# Test data
SVM.Test.A <- a[randIndex[(myCutPoint2_3+1):dim(a)[1]],]
View(SVM.Test.A)

# 1. Testing with ksvm model
SVM.Model.A <- ksvm(NPS_Type~Location, data = SVM.Train.A, kernel = "rbfdot", kpar = "automatic",
                           C=5, cross = 3, prob.model = TRUE)

# predicting the model
Predict.A <- predict(SVM.Model.A, SVM.Test.A)
SVM.Test.A$Predict <- Predict.A
tabulate(SVM.Test.A$Predict)
