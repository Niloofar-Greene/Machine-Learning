library(rpart)

train <- data.frame(
  ClaimID = c(1,2,3),
  RearEnd = c(TRUE, FALSE, TRUE),
  Fraud = c(TRUE, FALSE, TRUE)
)

mytree <- rpart(Fraud ~ RearEnd, data = train,  method = "class")
mytree

# Notice the output shows only a root node. This is because rpart has some default parameters that 
# prevented our tree from growing. Namely minsplit and minbucket. 
# minsplit = minimum number of observations that must exist in a node in order for a split to be attempted
# minbucket = minimum number of observations in any terminal node
# See what happens when we override these parameters.
# * means terminating leaf
 
mytree <- rpart(Fraud ~ RearEnd, data = train, method = "class", minsplit = 2, minbucket = 1)
mytree

# Now our tree has a root node, one split and two leaves (terminal nodes). Observe that rpart encoded 
# our boolean variable as an integer (false = 0, true = 1). 

# We can plot mytree by loading the rattle package (and some helper packages) and using the fancyRpartPlot() function.

library(rattle)
library(rpart.plot)
library(RColorBrewer)

# plot mytree
fancyRpartPlot(mytree, caption = NULL)
