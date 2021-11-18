library(readr)

data_credit <- read_csv("data_credit.csv")

######### colname ###############
names(data_credit)[17]='quality_target'

#########  decision tree ###############
library(rpart)

## to see the file
summary(data_credit)
str(data_credit)

# duration :lo=lower, up: up to. 
# purpose : why the person asked for the credit
# employment : years of employement
# quality target= If we decide to give money to the requester or not

# We like decision tree as it is easy to interpret it. It gives detailed explanation.However it does not have a high performance
# Becasue of ethiqual reasons, we remove some provate customers inforamtion (sex, foreign_worker)

## Natural error rate (NER)
# NER= 30% --> decision = tt le monde bon credit. on se trompe sur 30% des gens
NER= table(data_credit$quality_target)/nrow(data_credit) 

## Applying  classification model with Gini & Entropie
# Y (target) = quality_target
# split = gini  versus entropy 
# criterion go to control part
rpart_credit_gini=rpart(quality_target~., data_credit, parms = list(split='gini')
                        , control=list(maxdepth=10) , method ='class')            #Gini
rpart_credit_entropie=rpart(quality_target~., data_credit, parms = list(split='information')
                        , control=list(maxdepth=3) , method ='class')             #Entropy

## Model's summary
#This code returns an ensemble of observations packages. The most important packages include:
# variable importance : the higher the # , the more the variable has been used. if for example this value for 
# checking_status = 35% it means that if you don't have this variable, you lose 35% of variability of the tree. 
# you should re-scale it to 100, as the sum for all variables is not 100 necessarily.
# node number 1 : probabilities = NER : probabilities: 0.300 0.700 . we calculated it above 
summary(rpart_credit_gini) 

# The packages are not easily readable. Let's visualize then in a plot
plot(rpart_credit_gini)
text(rpart_credit_gini)

##Showing the rules and probabilities
rpart_credit_gini 
# 2) checking_status=<0,0<=X<200 543 240 good (0.4419890 0.5580110) meaning: good is coming from the value good in Y=quality_target
# the number to the left of "good" is the # of mis-classified cases . It shows if you subset data_credit based on 
# checking_status criteria <=0 and 0<=X<200, you will have 543 lines where 240 of them (always less than 50% of 543) are mis-classified
# in this case, mis-classified is "bad", because the majority is by "good"
# so, the judgment is "good" and the probability is 55% and we are making mistakes on 44% of cases of this leaf
# star(*) indicates the terminal leaves. you should take it from * and go up to see parents - the plot shows the same thing but in graph
A=subset(data_credit, checking_status %in% c("<0", "0<=X<200"))
B=subset(A, quality_target=="good")
C=subset(A, quality_target=="bad")
nrow(C)/nrow(A)
nrow(B)/nrow(A)

## predictability (good/bad) - we apply the rules on the training data
# column 17 is the dependent variable and we do not use it for prediction, so I remove it
y_prediction=predict(rpart_credit_gini, data_credit[,-17])
head(y_prediction,5)

nrow(y_prediction) == nrow(data_credit)

y_class=ifelse(data.frame(y_prediction)$good> 0.5, "good","bad")
y_class

# how many cases my model predicted as bad that are actually bad?
table(y_class, data_credit$quality_target)
# y_class bad good
#    bad  131   43
#    good 169  657

# same in %
table(y_class, data_credit$quality_target)/nrow(data_credit)


#Calculating the mis-classification rate (MCR) for Gini mEthod
# y_class   bad  good
#    bad  0.131 0.043
#    good 0.169 0.657
# TMC = 0.169 + 0.043 = 0.212 --> the mis-classification rate is 21% which is lower than the NER of 30% 
# --> the mis-classification rate has reduced by 2%  --> the model is acting well
A=table(y_class, data_credit$quality_target)/nrow(data_credit)
class(A)
A[c(2,3)]
MCR_gini= sum(A[c(2,3)])

NER[1]
MCR_gini
