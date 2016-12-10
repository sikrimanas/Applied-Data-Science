# NPS Value - Countries
# Calculating the NPS Value as per Countries list in the Data Set

# installing and loading the required packages
install.packages("VGAM")
library("VGAM")
install.packages("googleVis")
library("googleVis")
install.packages("gdata")
library("gdata")
install.packages("ggplot2")
library("ggplot2")
install.packages("gridExtra")
library("gridExtra")
install.packages("reshape2")
library("reshape2")

# The Original Data Set of January 2015
View(myData_Jan2015)

# Subsetting the Dataframe for NPS Value and Region value
RegionData <- myData_Jan2015[myData_Jan2015$G.Region_PL != "" & myData_Jan2015$NPS_Type != "",]
View(RegionData) # 38k entries

#########################################################################################################
########################################## Promoters ####################################################
Promoter <- RegionData[RegionData$NPS_Type == "Promoter",]
nrow(Promoter) # 26k entries
# calculating the number of promoters per countries
Promoter_Tabulate <- tapply(Promoter$COUNTRY_CODE_R, Promoter$COUNTRY_CODE_R, length)
View(Promoter_Tabulate)
Promoter_Tabulate <- data.frame(Promoter_Tabulate)
Promoter_Tabulate <- Promoter_Tabulate[-1,]
#colnames(Promoter_Tabulate) <- c("Promoters")

##########################################################################################################
########################################## Detractors ####################################################
Detractor <- RegionData[RegionData$NPS_Type == "Detractor",]
nrow(Detractor) # 4k entries
# calculating the number of detractors per countries
Detractor_Tabulate <- tapply(Detractor$COUNTRY_CODE_R, Detractor$COUNTRY_CODE_R, length)
View(Detractor_Tabulate)
Detractor_Tabulate <- data.frame(Detractor_Tabulate)
Detractor_Tabulate <- Detractor_Tabulate[-1,]

##########################################################################################################
########################################## Passive #######################################################
Passive <- RegionData[RegionData$NPS_Type == "Passive",]
nrow(Passive) # 7K entries
Passive_Tabulate <- tapply(Passive$COUNTRY_CODE_R, Passive$COUNTRY_CODE_R, length)
View(Passive_Tabulate)
Passive_Tabulate <- data.frame(Passive_Tabulate)
Passive_Tabulate <- Passive_Tabulate[-1,]

# Using Cbind to add the three columns
y <- cbind(Promoter_Tabulate, Detractor_Tabulate, Passive_Tabulate )
View(y)
y <- data.frame(y)
# resetting the clolumns names
colnames(y) <- c("Promoters", "Detracters", "Passive")
# cleaning the data, removing the last row which has some random numbers and column name
y <- y[-243,]
View(y)

# removing the NA values
CountryData <- na.omit(y)
View(CountryData) # 58 countries data
#changing the country type to DataFrame
CountryData <- data.frame(CountryData)

############################################ Percentage Calculations ################################
# percentage of Promoters specific to countries
str(CountryData)
CountryData$Total <- CountryData$Promoters + CountryData$Detracters + CountryData$Passive
CountryData$PromoterPercentage <- CountryData$Promoters / CountryData$Total * 100
CountryData$PromoterPercentage <- round(CountryData$PromoterPercentage, digits = 2)

# percentage of Promoters specific to countries
CountryData$DetractorPercentage <- CountryData$Detracters / CountryData$Total * 100
CountryData$DetractorPercentage <- round(CountryData$DetractorPercentage, digits = 2)

# percentage of Promoters specific to countries
CountryData$PassivePercentage <- CountryData$Passive / CountryData$Total * 100
CountryData$PassivePercentage <- round(CountryData$PassivePercentage, digits = 2)

# removing the last value
CountryData <- CountryData[-58,]
View(CountryData)

######################################################################################################
######################################################################################################
# bar Graph for Promoters
g1 <- ggplot(CountryData, aes(x = rownames(CountryData), y = PromoterPercentage)) +
  geom_bar(stat = "identity", colour = "black", fill = "darkolivegreen3") +
  theme(axis.text.x = element_text(angle = 90, hjust =1)) +
   xlab("Percentage of Promoters") + ylab("Countries") +
    ggtitle("Promoter Percentage vs Countries")

# Bar Graph for Detractors
g2 <- ggplot(CountryData, aes(x = rownames(CountryData), y = DetractorPercentage)) +
  geom_bar(stat = "identity", colour = "black", fill = "firebrick3") +
  theme(axis.text.x = element_text(angle = 90, hjust =1)) +
  xlab("Percentage of Detractor") + ylab("Countries") +
  ggtitle("Detractor Percentage vs Countries")

# combining the two plots into one
library("gridExtra")
grid.arrange(g1,g2)

######################################################################################################
################################ Projecting the NPS Value on WOrld Map ###############################

#colnames <- c("Country","Value", "Latitude", "Longitude")
CountryData$NPS <- CountryData$PromoterPercentage - CountryData$DetractorPercentage
View(CountryData)
str(CountryData)

# PLotting an Interactive Map
CountryData$Country <- rownames(CountryData)
G1 <- gvisGeoMap(CountryData,locationvar='Country',numvar='NPS',options=list(dataMode='regions'))

plot(G1) # plotting the map

# Bubble Chart
Country_New <- CountryData[-56,]
Bubble <- gvisBubbleChart(Country_New, idvar = "Country",
                          colorvar = "PromoterPercentage", sizevar = "DetractorPercentage",
                          options=list(
                            hAxis='{minValue:25, maxValue:75}'))
# plotting the bubble Chart
plot(Bubble)
View(CountryData)

####################################### End of Program ##############################################
#####################################################################################################