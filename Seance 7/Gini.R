library(readr)
#data_etud_note=read.csv('data_etudiants_notes.csv')
data_etud_notes <- read_delim("data_etudiants_notes.csv", ";", escape_double = FALSE, trim_ws = TRUE)
data_etud_socio <- read_csv("data_etudiants_socio_rv.csv")
data_credit <- read_delim("data_credit.csv", ";", escape_double = FALSE, trim_ws = TRUE)

str(data_etud_notes)

######### sort and order ###############
#rm(list=ls())

# opposite sort order
order(data_etud_socio$Frais_de_scolarite)
order(data_etud_socio$Frais_de_scolarite, decreasing = TRUE)

# difference between sort and order
# sort sorts by values, order sorts by order (order returns row number)
sort(data_etud_socio$Frais_de_scolarite)

sort(data_etud_socio$Frais_de_scolarite)[1]

# order data_etud_socio from low to high frais
data_etud_socio_order=data_etud_socio[order(data_etud_socio$Frais_de_scolarite),]
data_etud_socio_order[1,6]

######### aggregate ###############
aggregate(data_etud_socio,by=list(data_etud_socio$Provenance, data_etud_socio$Sexe), FUN="mean")

# Herve
aggregate(data_etud_socio[,c('Age','Frais_de_scolarite')],by=list(data_etud_socio$Provenance, data_etud_socio$Sexe), FUN="mean")
# Niloo
library(dplyr)
summarise(group_by(data_etud_socio, Provenance, Sexe), avg_age=mean(Age), avg_frais=mean(Frais_de_scolarite))

aggregate(data_etud_socio[,c('Age','Frais_de_scolarite')],by=list(data_etud_socio$Provenance, data_etud_socio$Sexe), FUN="max")
aggregate(data_etud_socio[,c('Age','Frais_de_scolarite')],by=list(data_etud_socio$Provenance, data_etud_socio$Sexe), FUN="min")

######### colname ###############
names(data_credit)[17]='quality_target'
essaie_data_etud_note=data_etud_notes[,-c(4,5)]
essaie_data_etud_note$f_m_diff=essaie_data_etud_note$Note_Francais-essaie_data_etud_note$Note_Math

# supprimer une colonne 
essaie_data_etud_note$f_m_diff<-NULL

######### arbre de decision ###############
library(rpart)
help("rpart")

## pr voir le fichier
summary(data_credit)

str(data_credit)
# duration :lo=lower, up: up to. 
# purpose : pq la personne demande le credit
# employment : annee d'employement
# quality target= oui on va donner de l'argent  a la personne ou pas

# on aime l'arbre de decision pour interpretation. il donne des explications detaillees, 
# mais cela n'est pas bon en performance
# selon les regles d'ethiques, il y a des coloumnes a enlever (sexe, travailleur etranger)

## taux naturel d'erreur (TNE)
TNE= table(data_credit$quality_target)/nrow(data_credit) 
# TNE= 30% --> decision = tt le monde bon credit. on se trompe sur 30% des gens

## Appliquer le modele de classification avec Gini et Entropie
# Y (target) = quality_target
# split = gini (gini) / entropie (information)
# raprt.control = criteres
rpart_credit_gini=rpart(quality_target~., data_credit, parms = list(split='gini')
                        , control=list(maxdepth=10) , method ='class')                #Gini
rpart_credit_entropie=rpart(quality_target~., data_credit, parms = list(split='information')
                            , control=list(maxdepth=3) , method ='class')             #Entropie

## sommaire de modele
summary(rpart_credit_gini) #ca retourne des packets d'obs. important package include:
# variable importance : the higher the # , the more the variable has been used. if for example this value for 
# checking_status = 35% it means that if you don't have this variable, you lose 35% of variability of the tree. 
# you should re-scale it in 100, as the sum for all variables is not 100 necessarily.
# node number 1 : probabilities = TNE : probabilities: 0.300 0.700 . we calculated it above by table

# ces packages ne sont pas lisibles --> on utilise plot
## affichier en graphique la presentation de modele
plot(rpart_credit_gini)
text(rpart_credit_gini)

##afficher les regles et presentation en terme de probabilite
rpart_credit_gini # pr voir les regles
# 2) checking_status=<0,0<=X<200 543 240 good (0.4419890 0.5580110) meaning: good is coming from the value good in Y=quality_target
# the number on the left of good is the # of the cases mal-classifie. It shows if you subset data_credit based on 
# checking_status criteria (0,0<=X<200), you will have 543 lines where 240 of them (always less than 50% of 543) are mal-classifie
# in this case, mal-classifie is "bad", because the majority is by "good"
# so, the judgment is "good" and the probability is 55% and we are making mistakes on 44% of cases of this leaf
#  checking_status=<0,0<=X<200 : rpart est un arbre de 2 brances (arbre binaire), ca veut pas dire qu'il va pas continuer jusqu'a fin
# * means terminal leaves. you should take it from * and go up to see parents - the plot shows the same thing but in graph
A=subset(data_credit, checking_status %in% c("<0", "0<=X<200"))
B=subset(A, quality_target=="good")
C=subset(A, quality_target=="bad")
nrow(C)/nrow(A)
nrow(B)/nrow(A)

## predictability (good/bad) - we apply the rules on the training data
# 17 est variable cible - on utilise pas variable cible pour predire
y_prediction=predict(rpart_credit_gini, data_credit[,-17])
y_prediction

nrow(y_prediction) == nrow(data_credit)

y_class=ifelse(data.frame(y_prediction)$good> 0.5, "good","bad")
y_class

# how many you predicted bad that are actually bad
# tableau croisee ppur en deduire le TMC
table(y_class, data_credit$quality_target)
# y_class bad good
#    bad  131   43
#    good 169  657

# same in %
table(y_class, data_credit$quality_target)/nrow(data_credit)
# y_class   bad  good
#    bad  0.131 0.043
#    good 0.169 0.657
# TMC = 0.169 + 0.043 = 0.212 --> on est passe de TNE de 30% a 21.2% --> 2% d'amelioration --> c bon
A=table(y_class, data_credit$quality_target)/nrow(data_credit)
class(A)
A[c(2,3)]
TMC_gini= sum(A[c(2,3)])

TNE[1]
TMC_gini
