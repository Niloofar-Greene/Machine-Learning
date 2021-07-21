library(readr)

data_credit <- read_delim("data_credit.csv", ";", escape_double = FALSE, trim_ws = TRUE)
names(data_credit)[17]='quality_target'

######### arbre de decision ###############
library(rpart)
help("rpart")

## taux naturel d'erreur (TNE)
TNE= table(data_credit$quality_target)/nrow(data_credit) 

## Appliquer le modele de classification avec  Entropie
# Y (target) = quality_target
# split =  entropie (information)
# raprt.control = criteres
rpart_credit_entropie=rpart(quality_target~., data_credit, parms = list(split='information')
                            , control=list(maxdepth=3) , method ='class') 

## sommaire de modele
summary(rpart_credit_entropie) 

## affichier en graphique la presentation de modele
plot(rpart_credit_entropie)
text(rpart_credit_entropie)

##afficher les regles et presentation en terme de probabilite
rpart_credit_entropie 

A=subset(data_credit, checking_status %in% c("<0", "0<=X<200"))
B=subset(A, quality_target=="good")
C=subset(A, quality_target=="bad")
nrow(C)/nrow(A)
nrow(B)/nrow(A)

## predictability (good/bad) 
# 17 est variable cible - on utilise pas variable cible pour predire
y_prediction=predict(rpart_credit_entropie, data_credit[,-17])
y_prediction

nrow(y_prediction) == nrow(data_credit)

y_class=ifelse(data.frame(y_prediction)$good> 0.5, "good","bad")

# how many you predicted bad that are actually bad
# tableau croisee ppur en deduire le TMC
table(y_class, data_credit$quality_target)

# same in %
table(y_class, data_credit$quality_target)/nrow(data_credit)

A=table(y_class, data_credit$quality_target)/nrow(data_credit)
class(A)
A[c(2,3)]
TMC_entropie= sum(A[c(2,3)])

TNE[1]
TMC_entropie
