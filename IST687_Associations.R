# NPS value - Associations
# Calculating the NPS Value as per Countries list in the Data Set

# Loading the required packages
install.packages("arules")
library("arules")
# creating a data frame for scaling
Association.Model <- myData_Jan2015
str(Association.Model)
View(Association.Model)

# creating a dataframe which contains NPS value
NPS.Association <- Association.Model[Association.Model$NPS_Type != "",]
View(NPS.Association)

# initializing an empty data frame
NPS.df <- data.frame(Doubles=double(),
                 Ints=integer(),
                 Factors=factor(),
                 Logicals=logical(),
                 Characters=character(),
                 stringsAsFactors=FALSE)

# subsetting the original data frame
NPS.df <- subset(NPS.Association, select = c("POV_CODE_C", "COUNTRY_CODE_R", "GUEST_COUNTRY_R",
                                   "MEMBER_STATUS_R", "e_country_I", "Brand_PL", "Region_PL",
                                   "Location_PL", "GP_Tier", "Channel_Category", "Booking_Channel",
                                   "NPS_Type"))
View(NPS.df) # verifying the data frame values

str(NPS.df) # check if all the columns are either numeric or factor
# all are factors

# creating rules with respect to NPS_type
Rules.NPS <- apriori(NPS.df, parameter = list(support = 0.2, confidence =0.5),
                     appearance = list(rhs=c("NPS_Type=Promoter", "NPS_Type=Detractor",
                                             "NPS_Type=Passive"), default="lhs"))

Rules.NPS

# good rules
Good.Rules <- Rules.NPS[quality(Rules.NPS)$lift>1]
inspect(Good.Rules)

plot(Good.Rules)
plot(Good.Rules, method = "graph", control=list(type="items"))
plot(Good.Rules, method = "paracoord", control=list(reorder=TRUE))
plot(Good.Rules, method = "matrix", measure = "lift", control=list(reorder=TRUE))
plot(Good.Rules, method = "matrix3D", measure = "lift", control=list(reorder=TRUE))

###################################################################################################
###################################################################################################

#Assocaitive Rules with respect to Region - "Asia Pacific"

NPS.Region.df <- NPS.df[NPS.df$Region_PL == "Asia Pacific",]
nrow(NPS.Region.df)

Rules.Region <- apriori(NPS.Region.df, parameter = list(support = 0.2, confidence =0.5),
                     appearance = list(rhs=c("NPS_Type=Promoter", "NPS_Type=Detractor",
                                             "NPS_Type=Passive"), default="lhs"))
inspect(Rules.Region)

# plot
plot(Rules.Region)
plot(Rules.Region, method = "graph", control=list(type="items"))
plot(Rules.Region, method = "paracoord", control=list(reorder=TRUE))
plot(Rules.Region, method = "matrix", measure = "lift", control=list(reorder=TRUE))
plot(Rules.Region, method = "matrix3D", measure = "lift", control=list(reorder=TRUE))


####################################################################################################
####################################################################################################

# to find associative rules for facilities
# to calculate the index for specific columns
indexStart <- grep("All.Suites.PL", colnames(NPS.Association))
indexEnd <- grep("Valet.Parking_PL", colnames(NPS.Association))

NPS.Facilities <- NPS.Association[,indexStart:indexEnd]
NPS.Facilities$NPS_Type <- NPS.Association$NPS_Type
View(NPS.Facilities)

Rules.Facilities <- apriori(NPS.Facilities, parameter = list(support = 0.60, confidence =0.65),
                            appearance = list(rhs=c("NPS_Type=Promoter", "NPS_Type=Detractor",
                                                    "NPS_Type=Passive"), default="lhs"))
Rules.Facilities
inspect(Rules.Facilities) # finally some success

plot(Rules.Facilities, method = "graph", control=list(type="items"))
plot(Rules.Facilities, method = "paracoord", control=list(reorder=TRUE))
plot(Rules.Facilities, method = "matrix", measure = "lift", control=list(reorder=TRUE))
plot(Rules.Facilities, method = "matrix3D", measure = "lift", control=list(reorder=TRUE))

####################################### End of Program ##############################################
#####################################################################################################

NPS.Detractors <- NPS.Facilities[NPS.Facilities$NPS_Type == "Detractor",]
nrow(NPS.Detractors)

Rules.Facilities2 <- apriori(NPS.Detractors, parameter = list(support = 0.90, confidence =1),
                            appearance = list(rhs=c("NPS_Type=Detractor", "NPS_Type=Promoter",
                                                    "NPS_Type=Passive"), default="lhs"))
inspect(Rules.Facilities2)


#####################################################################################################
################################## Associations for Likelyhood to Recommend #########################

Association.Jan <- read.csv("file:///D:/Syracuse Subjects/IST687 - Applied Data Science/Project/out-hyatt_Jan2015.csv")
View(Association.Jan)
nrow(Association.Jan)
str(Association.Jan)

# removing the rows with NA values and taking only rows which have some data in likelihood to recommend
# subset function
Association.Jan.NPS <- subset(Association.Jan, Likelihood_Recommend_H != "NA")
nrow(Association.Jan.NPS)

indexStart1 <- grep("Likelihood_Recommend_H", colnames(Association.Jan.NPS))
indexEnd1 <- grep("Internet_Sat_H", colnames(Association.Jan.NPS))


Subset.Jan.NPS <- Association.Jan.NPS[,60:67]
str(Subset.Jan.NPS)
# as factor
Subset.Jan.NPS$Likelihood_Recommend_H <- as.factor(Subset.Jan.NPS$Likelihood_Recommend_H)
Subset.Jan.NPS$Overall_Sat_H <- as.factor(Subset.Jan.NPS$Overall_Sat_H)
Subset.Jan.NPS$Guest_Room_H <- as.factor(Subset.Jan.NPS$Guest_Room_H)
Subset.Jan.NPS$Tranquility_H <- as.factor(Subset.Jan.NPS$Tranquility_H)
Subset.Jan.NPS$Condition_Hotel_H <- as.factor(Subset.Jan.NPS$Condition_Hotel_H)
Subset.Jan.NPS$Customer_SVC_H <- as.factor(Subset.Jan.NPS$Customer_SVC_H)
Subset.Jan.NPS$Staff_Cared_H <- as.factor(Subset.Jan.NPS$Staff_Cared_H)
Subset.Jan.NPS$Internet_Sat_H <- as.factor(Subset.Jan.NPS$Internet_Sat_H)

str(Subset.Jan.NPS)
# analyzing the rules
Rules.Likelihood <- apriori(Subset.Jan.NPS, parameter = list(support = 0.2, confidence = 0.9),
                            appearance = list(rhs=c("Likelihood_Recommend_H=10", "Likelihood_Recommend_H=9",
                                                    "Likelihood_Recommend_H=8"), default="lhs"))

inspect(Rules.Likelihood) # 10 rules
plot(Rules.Likelihood)
plot(Rules.Likelihood, method = "graph", control=list(type="items"))
plot(Rules.Likelihood, method = "paracoord", control=list(reorder=TRUE))
plot(Rules.Likelihood, method = "matrix", measure = "lift", control=list(reorder=TRUE))
plot(Rules.Likelihood, method = "matrix3D", measure = "lift", control=list(reorder=TRUE))
