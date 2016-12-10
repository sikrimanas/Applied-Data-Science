#Linear modeling  


  #loading the dataset
  datag <-read.csv("D:/IST-687/out-hyatt_Jan2015.csv")

  #View(datag)
  #summary(datag)
  
  
  #considering only the folllowing columns in the dataset:Likelihood_Recommend_H, Tranquility_H, Condition_Hotel_H
  data1 <- datag[,which(names(datag) %in% c("Likelihood_Recommend_H","Customer_SVC_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H", "Condition_Hotel_H", "Staff_Cared_H", "Internet_Sat_H"))]
  View(data1)
  summary(data1)
  #deleting records having NA from the dataset
  data1<-na.omit(data1)
  nrow(data1)
  
  #### Now creating a linear model between likelihood to recommend and Customer Service
  linearModel_1 <- lm(formula = data1$Likelihood_Recommend_H~data1$Customer_SVC_H, data = data1)
  
  # Checking the R-squared value for this model to see how well CustomerService can predict ability to recommend
  summary(linearModel_1) #R-Squared - 0.4799
  
  #Plotting the linear model
  plot(linearModel_1)
  
  
  #Drawing the abline
  abline(linearModel_1)
  
  
  #### Now creating a linear model between likelihood to recommend and Overall Satisfaction
  linearModel_2 <- lm(formula = data1$Likelihood_Recommend_H~data1$Overall_Sat_H, data = data1)
  
  # Checking the R-squared value for this model to see how well Overall satisfaction can predict ability to recommend
  summary(linearModel_2) #R-Squared - 0.799
  
  #Plotting the linear model
  plot(linearModel_2)
  
  
  #Drawing the abline
  abline(linearModel_2)
  
  
  
  #### Now creating a linear model between likelihood to recommend and Guest Room
  linearModel_3 <- lm(formula = data1$Likelihood_Recommend_H~data1$Guest_Room_H, data = data1)
  
  # Checking the R-squared value for this model to see how well Guest Room can predict ability to recommend
  summary(linearModel_3) #R-Squared - 0.4856
  
  #Plotting the linear model
  plot(linearModel_3)
  
  
  #Drawing the abline
  abline(linearModel_3)
  
  
  
  #### Now creating a linear model between likelihood to recommend and Tranquality
  linearModel_4 <- lm(formula = data1$Likelihood_Recommend_H~data1$Tranquility_H, data = data1)
  
  # Checking the R-squared value for this model to see how well Tranquility can predict ability to recommend
  summary(linearModel_4) #R-Squared - 0.3444
  
  #Plotting the linear model
  plot(linearModel_4)
  
  
  #Drawing the abline
  abline(linearModel_4)
  
  
  
  #### Now creating a linear model between likelihood to recommend and Condition of Hotel
  linearModel_5 <- lm(formula = data1$Likelihood_Recommend_H~data1$Condition_Hotel_H, data = data1)
  
  # Checking the R-squared value for this model to see how well Condition of Hotel can predict ability to recommend
  summary(linearModel_5) #R-Squared - 0.446
  
  #Plotting the linear model
  plot(linearModel_5)
  
  
  #Drawing the abline
  abline(linearModel_5)
  
  
  
  #### Now creating a linear model between likelihood to recommend and Staff cared
  linearModel_6 <- lm(formula = data1$Likelihood_Recommend_H~data1$Staff_Cared_H, data = data1)
  
  # Checking the R-squared value for this model to see how well Staff Cared can predict ability to recommend
  summary(linearModel_6) #R-Squared - 0.4035
  
  #Plotting the linear model
  plot(linearModel_6)
  
  
  #Drawing the abline
  abline(linearModel_6)
  
  
  
  #### Now creating a linear model between likelihood to recommend and Internet Satisfaction
  linearModel_7 <- lm(formula = data1$Likelihood_Recommend_H~data1$Internet_Sat_H, data = data1)
  
  # Checking the R-squared value for this model to see how well Internet Satisfaction can predict ability to recommend
  summary(linearModel_7) #R-Squared - 0.08714
  
  #Plotting the linear model
  plot(linearModel_7)
  
  
  #Drawing the abline
  abline(linearModel_7)
  
  
  
  #### Now creating a linear model between likelihood to recommend and everything combines
  linearModel_8 <- lm(formula = data1$Likelihood_Recommend_H~data1$Overall_Sat_H + data1$Guest_Room_H + data1$Tranquility_H + data1$Condition_Hotel_H + data1$Customer_SVC_H + data1$Staff_Cared_H + data1$Internet_Sat_H, data = data1)
  
  # Checking the R-squared value for this model to see how well Overall Satisfaction can predict ability to recommend
  summary(linearModel_8) #R-Squared - 0.8113
  
  #Plotting the linear model
  plot(linearModel_8)
  
  
  #Drawing the abline
  abline(linearModel_8)
  
  
  