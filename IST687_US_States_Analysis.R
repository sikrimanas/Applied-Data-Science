# NPS Value - US States

# Installing and Loading the required packages
install.packages("ggplot2")
library("ggplot2")
install.packages("gridExtra")
library("gridExtra")
install.packages("googleVis")
library("googleVis")
install.packages("maps")
library("maps")
install.packages("mapsdata")
library("mapsdata")

# loading the csv file
myData_Jan2015 <- read.csv("file:///D:/Syracuse Subjects/IST687 - Applied Data Science/Project/out-hyatt_Jan2015.csv")

# Original Data Set
View(myData_Jan2015) 
nrow(myData_Jan2015) # checking the number of rows

######################################################################################################
##################################### Country - USA #################################################3
Country.US <- myData_Jan2015[myData_Jan2015$COUNTRY_CODE_R == 'UNITED STATES',]
View(Country.US) #678,830 entries

# tabulate and check how many blank values are present 
unique(Country.US$STATE_R)
View(Country.US)

# resetting the rownames
rownames(Country.US) <- NULL
# Checking the column that had blank and assigning it to a variable
noData <- Country.US[42,18]
# removing rows with no data
Country.US <- Country.US[Country.US$STATE_R != noData,]
View(Country.US) # 674,827 entries

str(Country.US$STATE_R)

unique(Country.US$STATE_R)

zero <- Country.US[Country.US$STATE_R == '0',]
View(zero)

tabulate(Country.US$STATE_R)
View(myDataFrame)

######################################################################################################
####################################### States - Revenue #############################################
Revenue.Analysis <- tapply(Country.US$REVENUE_USD_R, Country.US$STATE_R, sum)
View(Revenue.Analysis)
# framing the values in a dataframe
Revenue.Analysis <- data.frame(Revenue.Analysis)
# omitting the data
Revenue.Analysis <- na.omit(Revenue.Analysis)
View(Revenue.Analysis)
# creating a new column with states names
Revenue.Analysis$State.Name <- rownames(Revenue.Analysis)
# Only US States are left now
Revenue.Analysis <- Revenue.Analysis[Revenue.Analysis$State.Name %in% state.abb,]

# function to change abbreviations to state name
# adding state name to abbreviations
abbr2state <- function(abbr){
  ab <- tolower(c("AL", "AK", "AZ", "KS", "UT", "CO", "CT",
                  "DE", "FL", "GA", "HI", "ID", "IL",
                  "IN", "IA", "AR", "KY", "LA", "ME",
                  "MD", "MA", "MI", "MN", "MS", "MO",
                  "MT", "NE", "NV", "NH", "NJ", "NM",
                  "NY", "NC", "ND", "OH", "OK", "OR",
                  "PA", "RI", "SC", "SD", "TN", "TX",
                  "CA", "VT", "VA", "WA", "WV", "WI",
                  "WY", "DC"))
  st<- c("Alabama",
         "Alaska", "Arizona", "Kansas",
         "Utah", "Colorado", "Connecticut",
         "Delaware", "Florida", "Georgia",
         "Hawaii", "Idaho", "Illinois",
         "Indiana", "Iowa", "Arkansas",
         "Kentucky", "Louisiana", "Maine",
         "Maryland", "Massachusetts", "Michigan",
         "Minnesota", "Mississippi", "Missouri",
         "Montana", "Nebraska", "Nevada",
         "New Hampshire", "New Jersey", "New Mexico",
         "New York", "North Carolina", "North Dakota",
         "Ohio", "Oklahoma", "Oregon",
         "Pennsylvania", "Rhode Island", "South Carolina",
         "South Dakota", "Tennessee", "Texas",
         "California", "Vermont", "Virginia",
         "Washington", "West Virginia", "Wisconsin",
         "Wyoming", "District of Columbia")
  st[match(tolower(abbr), ab)]
}

# creating a vector to store the statenames as per the abbreviations
myStatename <- abbr2state(Revenue.Analysis$State.Name)
Revenue.Analysis <- data.frame(Revenue.Analysis, myStatename)
View(Revenue.Analysis)
# change the statename to lower
Revenue.Analysis$myStatename <- tolower(Revenue.Analysis$myStatename)
colnames(Revenue.Analysis) <- c("Revenue", "State.Abb", "State.Name")

######################################################################################################
############################################ plotting US map #########################################

# specify the mapdata to be defined by state
us <- map_data("state")

# creating the U.S. map, representing the color with the total revenue of that state
ggplot(Revenue.Analysis, aes(map_id = State.Name)) +
  geom_map(map = us, aes(fill = Revenue), color = "black") +
  scale_fill_gradient(palette = "PuBu") +
    expand_limits(x = us$long, y = us$lat) +
      coord_map() + ggtitle("Total Revenue per State")

# different types of plot
library("googleVis")

require(datasets)

GeoStates <- gvisGeoChart(Revenue.Analysis, "State.Name", "Revenue",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       color = "Yellow",
                                       width=600, height=400))
plot(GeoStates)

#######################################################################################################
####################################### Top 3 States vs Bottom 3 States ###############################
#maximum revenue
Revenue[which.max(Revenue.Analysis$Revenue),]
View(Revenue.Analysis)

## To calculate types of location with each state
State.Length <- tapply(Country.US$STATE_R, Country.US$STATE_R, length)
View(State.Length)

State.Length$State.Name <- rownames(State.Length)
State.Length <- na.omit(State.Length)

State.Length <- data.frame(State.Length)
State.Length <- State.Length[State.Length$State.Name %in% state.abb,]
View(State.Length)

# Adding number of entries to revenue data frame
Revenue.Analysis$No.of.Entries <- State.Length$State.Length
View(Revenue.Analysis)

# perform sorting in decreasing and increasing order as per revenue
Revenue.Analysis2 <- Revenue.Analysis[order(Revenue.Analysis$Revenue),]
View(Revenue.Analysis2)

# Top 3 states as per revenue
top.States <- Revenue.Analysis2[48:50,]
# Botton 3 states as per revenue
Bottom.States <- Revenue.Analysis2[1:3,]
View(top.States)
View(Bottom.States)

# subsetting dataframe for Top 3 states
Top3 <- subset(myData_Jan2015, myData_Jan2015$STATE_R == "CA" | myData_Jan2015$STATE_R == "TX" | myData_Jan2015$STATE_R == "IL")
View(Top3)

tabulate(Top3$NPS_Type)
unique(Top3$NPS_Type)
# subsetting dataframe for Bottom 3 states
Bottom3 <- subset(myData_Jan2015, myData_Jan2015$STATE_R == "SD" | myData_Jan2015$STATE_R == "VT" | myData_Jan2015$STATE_R == "WY")
View(Bottom3)

tabulate(Bottom3$NPS_Type)
unique(Bottom3$NPS_Type)

# number of promoters, demoters, passive in top 3 states
a <- nrow(Top3[Top3$STATE_R == "CA" & Top3$NPS_Type == "Promoter",])

b <- nrow(Top3[Top3$STATE_R == "CA" & Top3$NPS_Type == "Detractor",])
c <- nrow(Top3[Top3$STATE_R == "CA" & Top3$NPS_Type == "Passive",])

top.States$Promoter <- 0
top.States$Detractor <- 0
top.States$Passive <- 0
View(top.States)

a1 <- nrow(Top3[Top3$STATE_R == "TX" & Top3$NPS_Type == "Promoter",])
b1 <- nrow(Top3[Top3$STATE_R == "TX" & Top3$NPS_Type == "Detractor",])
c1 <- nrow(Top3[Top3$STATE_R == "TX" & Top3$NPS_Type == "Passive",])

a2 <- nrow(Top3[Top3$STATE_R == "IL" & Top3$NPS_Type == "Promoter",])
b2 <- nrow(Top3[Top3$STATE_R == "IL" & Top3$NPS_Type == "Detractor",])
c2 <- nrow(Top3[Top3$STATE_R == "IL" & Top3$NPS_Type == "Passive",])

# add the values to the top.states data frame
top.States$Promoter <- c(a2,a1,a)
top.States$Detractor <- c(b2,b1,b)
top.States$Passive <- c(c2,c1,c)
top.States$total <- top.States$Promoter + top.States$Detractor + top.States$Passive

# number of promoters, demoters, passive in bottom 3 states
a3 <- nrow(Bottom3[Bottom3$STATE_R == "SD" & Bottom3$NPS_Type == "Promoter",])
b3 <- nrow(Bottom3[Bottom3$STATE_R == "SD" & Bottom3$NPS_Type == "Detractor",])
c3 <- nrow(Bottom3[Bottom3$STATE_R == "SD" & Bottom3$NPS_Type == "Passive",])

Bottom.States$Promoter <- 0
Bottom.States$Detractor <- 0
Bottom.States$Passive <- 0
View(Bottom.States)

a4 <- nrow(Bottom3[Bottom3$STATE_R == "VT" & Bottom3$NPS_Type == "Promoter",])
b4 <- nrow(Bottom3[Bottom3$STATE_R == "VT" & Bottom3$NPS_Type == "Detractor",])
c4 <- nrow(Bottom3[Bottom3$STATE_R == "VT" & Bottom3$NPS_Type == "Passive",])

a5 <- nrow(Bottom3[Bottom3$STATE_R == "WY" & Bottom3$NPS_Type == "Promoter",])
b5 <- nrow(Bottom3[Bottom3$STATE_R == "WY" & Bottom3$NPS_Type == "Detractor",])
c5 <- nrow(Bottom3[Bottom3$STATE_R == "WY" & Bottom3$NPS_Type == "Passive",])

# add the values to the Bottom.States data frame
Bottom.States$Promoter <- c(a3,a4,a5)
Bottom.States$Detractor <- c(b3,b4,b5)
Bottom.States$Passive <- c(c3,c4,c5)
Bottom.States$total <- Bottom.States$Promoter + Bottom.States$Detractor + Bottom.States$Passive

#######################################################################################################
####################################### Plotting Top3 States ##########################################

# combining top 3 states promoter, detractor and passive
top3.promoter <- subset(Top3, Top3$NPS_Type == "Promoter")
top3.detractor <- subset(Top3, Top3$NPS_Type == "Detractor")
top3.passive <- subset(Top3, Top3$NPS_Type == "Passive")

top3.final <- rbind(top3.promoter,top3.detractor,top3.passive)
View(top3.final)
#plotting
q1 <- qplot(STATE_R, data = top3.final, geom = "bar", fill = factor(NPS_Type),
            xlab = "State Names", ylab = "Count")
q1 <- q1 + labs(title = "Top 3 Revenue States USA")

#######################################################################################################
####################################### Plotting Bottom 3 States ######################################

# combining bottom 3 states promoter, detractor and passive
Bottom3.promoter <- subset(Bottom3, Bottom3$NPS_Type == "Promoter")
Bottom3.detractor <- subset(Bottom3, Bottom3$NPS_Type == "Detractor")
Bottom3.passive <- subset(Bottom3, Bottom3$NPS_Type == "Passive")

bottom3.final <- rbind(Bottom3.promoter,Bottom3.detractor,Bottom3.passive)
# plotting
q2 <- qplot(STATE_R, data = bottom3.final, geom = "bar", fill = factor(NPS_Type),
            xlab = "State Names", ylab = "Count")
q2 <- q2 + labs(title = "Bottom 3 Revenue States USA")

# combing the two plots in the same frame
library("gridExtra") # required library
grid.arrange(q1,q2, nrow =1) # plot


#######################################################################################################
#################################### Top3 NPS and Bottom 3 NPS ########################################

Country.US.NPS <- Country.US[Country.US$NPS_Type != "",]
nrow(Country.US.NPS) # 26943 entries
View(Country.US.NPS) 

tabulate(Country.US.NPS$NPS_Type)
unique(Country.US.NPS$NPS_Type)

###################### Promoters Subset #####################################
NPS.Promoters <- subset(Country.US.NPS, NPS_Type == "Promoter")
nrow(NPS.Promoters) # 18723 entries

a <- tapply(NPS.Promoters$STATE_R, NPS.Promoters$STATE_R, length)
a <- data.frame(a)
View(a)
a <- na.omit(a)

# Only US States are left now
a <- a[rownames(a) %in% state.abb,]
a <- data.frame(a)
str(a)
colnames(a) <- c("Promoter")
a <- a[-1,]

################# Detractor Subsetb##############################################
NPS.Detractor <- subset(Country.US.NPS, NPS_Type == "Detractor")
nrow(NPS.Detractor) # 3336 entries

b <- tapply(NPS.Detractor$STATE_R, NPS.Detractor$STATE_R, length)
b <- data.frame(b)
View(b)
b <- na.omit(b)

# Only US States are left now
b <- b[rownames(b) %in% state.abb,]
b <- data.frame(b)
str(b)
colnames(b) <- c("Detractor")

################################ Passive subset##############################
NPS.Passive <- subset(Country.US.NPS, NPS_Type == "Passive")
nrow(NPS.Passive) # 4884 Entries

c <- tapply(NPS.Passive$STATE_R, NPS.Passive$STATE_R, length)
c <- data.frame(c)
View(c)
c <- na.omit(c)
c <- c[-1,]

# Only US States are left now
c <- c[rownames(c) %in% state.abb,]
c <- data.frame(c)
str(c)
colnames(c) <- c("Passive")

######################## combining the data frames#############################
NPS.State.df <- data.frame(a,b,c)
View(NPS.State.df)
# adding a state abbreviations column to data frame
NPS.State.df$State_Abb <- rownames(NPS.State.df)
#changing the state abbreviations to state name
State_Name <- abbr2state(NPS.State.df$State_Abb)
NPS.State.df <- data.frame(NPS.State.df, State_Name)

# calculating the total amount
NPS.State.df$Total <- NPS.State.df$Promoter + NPS.State.df$Detractor + NPS.State.df$Passive

# Promoter Percentage
NPS.State.df$Promoter_Percentage <- NPS.State.df$Promoter/NPS.State.df$Total * 100
NPS.State.df$Promoter_Percentage <- round(NPS.State.df$Promoter_Percentage) # rounding off the value

# Detractor Percentage
NPS.State.df$Detractor_Percentage <- NPS.State.df$Detractor/NPS.State.df$Total * 100
NPS.State.df$Detractor_Percentage <- round(NPS.State.df$Detractor_Percentage) # rounding off the value

# NPS for States
NPS.State.df$NPS <- NPS.State.df$Promoter_Percentage - NPS.State.df$Detractor_Percentage
View(NPS.State.df)

# Top 3 NPS State
# Montana - MT
# Nebraska - NE
# Tennessee - TN

# Bottom 3 NPS State
# North Dakota - ND
# Rhode Island - RI
# Wyoming - WY

# specify the mapdata to be defined by state
us <- map_data("state")

# change the state name to lower case
NPS.State.df$State_Name <- tolower(NPS.State.df$State_Name)

# creating the U.S. map, representing the color with the total revenue of that state
ggplot(NPS.State.df, aes(map_id = State_Name)) +
  geom_map(map = us, aes(fill = NPS), color = "black") +
   scale_fill_gradient(palette = "PuBu") +
    expand_limits(x = us$long, y = us$lat) +
     coord_map() + ggtitle("NPS per State")

View(NPS.State.df)

# different types of plot
library("googleVis")

require(datasets)

GeoStates <- gvisGeoChart(NPS.State.df, "State_Name", "NPS",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       color = "Yellow",
                                       width=600, height=400))
plot(GeoStates)
################################## End of Program #################################################
###################################################################################################