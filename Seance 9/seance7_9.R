library(readr)
library(rpart)
library(adabag) # Pour le Bagging et le Boosting
library(randomForest) # Pour les forets aleatoires

data_credits <- read_csv("data_credit.csv")
head(data_credits)
head(data_credits$class)

########################################
summary(data_credits)
class(data_credits)

# Calculer notre d'erreur naturel# est ce que la personne est un mauvais paiyeur ou non. mauvais payeur = 1, sinon = 0
 # bad % = TNE (taux naturel erreur)
table(data_credits$class)/nrow(data_credits)

#appliquerle modele de classification - gini
rpart_credit_gini=rpart(class~.,data_credits,parms = list(split='gini'),control = list(maxdepth=10))
#appliquerle modele de classification - entropie
rpart_credit_entropy=rpart(class~.,data_credits,parms = list(split='information'),control = list(maxdepth=3))

#sommaire du modele
summary(rpart_credit_gini)
#affiche en graphique la representation du modele
plot(rpart_credit_gini)
text(rpart_credit_gini)

#afficher les regle
rpart_credit_gini

#Prediction sur toute notre table de donnees
y_pred=predict(rpart_credit_gini,data_credits)
y_pred #probabilite

#Si la probabilite pour good est superieur a 50%, dans ce cas cela sera la valeur good et sinon ca sera la valeur bad
y_class=ifelse(data.frame(y_pred)$good>0.5,'good','bad')

#Tableau croise pour en deduire le taux de mauvaise classification
table(y_class,data_credits$class)/nrow(data_credits)

############################################ Methode d'ensemble ############################################
# Calculer notre d'erreur naturel
table(data_credits$class)/nrow(data_credits)
TNE=table(data_credits$class)/nrow(data_credits)

################################### Bagging ################################
# pour bagging, boosting et foret aleatoire les colonnes de character doivent etre des facteurs
data_credits <- data_credits %>% mutate_if(sapply(data_credits, is.character), as.factor)
summary(data_credits)

# et type de table doit etre data.frame
data_credits=data.frame(data_credits)
class(data_credits)

bagging_credit=bagging(class~.,data_credits,mfinal=100)
summary(bagging_credit)

# pour voir la classe predites
bagging_credit$class
# pour TMC - on fait table croise avec table predit et table originale (obs de class)
# pour 33 cas, modele a predit "bad" alors que en realite etait "good" - 667 sont predit comme good et sont good en realite
table(bagging_credit$class, data_credits$class)

# les obs bien predites par le modele
diag(table(bagging_credit$class, data_credits$class))

# le nombre total ou le modele a bien classifie
SUMM=sum(diag(table(bagging_credit$class, data_credits$class)))

# taux de bonne classification - 83.6%
TBC=SUMM/nrow(data_credits)
# TMC = 16.4%
TMC= 1-TBC
# TMC = 16.4% vs. TNE = 30%

#IMPORTANCE DES VARIABLES (poids) - checking status a le poids le plus eleve de 31,89
bagging_credit$importance

# inconveniant de methode d'ensemble = on perd enormement en explication de predictions, mais ils vont gagner par rapport aux arbres, 
# mais les arbres peuvent donner des explication

####### ############################ Boosting ################################
boosting_credit=boosting(class~.,data_credits,mfinal=101,control=rpart.control(maxdepth=5))

# weight = poind des arbres = 1/TMC
summary(boosting_credit)

# Importance des variables Boosting
# poids des arbres. 0.55 est le meilleurs
boosting_credit$weights

# les obs bien predites par le modele
diag(table(boosting_credit$class, data_credit$class))
# le nombre total ou le modele a bien classifie
SUMM=sum(diag(table(boosting_credit$class, data_credit$class)))
# taux de bonne classification 
TBC=SUMM/nrow(data_credits)
# TMC Boosting 
# TMC = 4.3% vs TNE = 30%
TMC= 1-TBC

# variables important --> purpose
boosting_credit$importance

# Decision de choix de modele entre Bagging et Boosting: 
# TMC Bagging : 16% vs. TMC Boosting = 4% --> Selon le TMC, Boosting est meilleur

#################################### Foret aleatoire ################################
# ntree= # abrbre, mtry= # des variable explivative tirees aleatoirement
rf_credit=randomForest(class~.,data_credits,ntree=101,mtry=5)

# predicted (au lieu de class), err_rate, obb_times : # de temps que la personne qui etait pas tirees dans le OBB,  
# importance, y = var cible
summary(rf_credit)

# TMC = 24.3% vs. TNE = 30%
1-sum(diag(table(rf_credit$predicted, data_credits$class)))/nrow(data_credits)

# importance - checking_status
rf_credit$importance

#### TMC des 3 methodes - bosting est best
1-sum(diag(table(bagging_credit$class, data_credits$class)))/nrow(data_credits)
1-sum(diag(table(boosting_credit$class, data_credits$class)))/nrow(data_credits)
1-sum(diag(table(rf_credit$predicted, data_credits$class)))/nrow(data_credits)

# on n'a pas encore valide le SA de boosting et bagging
# oui booting est meilleur mais ca se peut qu il y a un grand SA  ici. 
# ce TMC de 4% pourrait augementer a 25% apres corriger le SA.
# valider la performce des modeles au niveau de SA : TMC de Foret Aleatoire et TMC de fichier validation et voir la difference
# si l y a SA sur Boosting : soit on reduit le nombre des arbres et profondeur pour voir ce qui va se passer (faire un tune)
# si tune marche pas, soit on abandonne le modele soit on travaille sur les variables explicative

# TMC de Boosting augememnt a 15% quand on diminie profondeur de 5 a 3
boosting_credit=boosting(class~.,data_credits,mfinal=101,control=rpart.control(maxdepth=3))
# donc, peut-etre a profondeur = 5 il y avait un SA. on va voir la semaine prochain
# car a 5 est plus complexe , car on fait plus de calcul alors que l'ARBbre doivent etre simple (faibles)
# meilleurs profondeur est entre 3 et 5
# on peut pas choisit 1000 arbre car on doir respecer l'idee d emethode d'ensemble
# blending est time-consuming
