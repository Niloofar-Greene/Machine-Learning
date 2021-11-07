library(readr)
library(rpart)
library(adabag) # Pour le Bagging et le Boosting
library(randomForest) # Pour les forets aleatoires

#data_credits=read.csv("C:\\Users\\hmensah\\Documents\\Documents\\Cours_IED\\data_credit.csv",sep=';',colClasses ='factor')
data_credits=data_credit <- read_delim("data_credit.csv",";", escape_double = FALSE, trim_ws = TRUE)
A=data_credit[1:5,c(1:3, 17)]

################################### Bagging ################################
bagging_credit=bagging(class~.,A,mfinal=2)
################################### Boosting ################################
boosting_credit=boosting(class~.,A,mfinal=3,control=rpart.control(maxdepth=2))
