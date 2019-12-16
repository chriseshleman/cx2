
setwd("~/Dropbox/Work and research/Port Authority/cx2") 
library(readxl) 
library(mice) 
library(dplyr) 
library(Hmisc) 
library(imputeMissings) 
library(ggplot2) 
library(beepr) 
library(ggdendro) 
library(reshape2) 
library(tidyr) 
library(remotes) 
#remotes::install_github("njtierney/naniar") 
library(naniar) 
library(tidyverse) 
library(tree) 
library(rpart) 
library(rpart.plot)
library(expss) 

rm(list = ls()) # clear global environment 
cat("\014") # clear the console 
options(warn=-1) # suppress annoying warnings 

pmiss = function(x){sum(is.na(x))/length(x)*100} 


# What to do? This is Part 2, basic tree. Part 1 was an NA analysis and Part 3 can be advanced tree methods (bagging / random forest). 

#1. Load data and munge - classify 
#2. Tree JDP classification 
#3. Interpret 
#4. Cross-validate 
#5. Interpret 
#6. Repeat ASQ
#7. Random forest 

###1. Load data and munge - classify 
asq193 = as.data.frame(read_excel("./ACI - ASQ/2019 Q3/ACI ASQ Survey Main_ Q3 2019 Data-EXCEL-v1.xlsx"))
names(asq193) = tolower(names(asq193)) 

jdp = read.csv("./JDPower18_noNA.csv") 
jdp.index = jdp$X 
jdp$X = NULL 
jdp$high = ifelse(jdp$Overall.Satisfaction.Index > 900, "high", "low") 
  # Consider Jenks (natural) breaks instead next time. 

names(jdp) 
summary(jdp$Overall.Satisfaction.Index) 
# Subtract airport, state, RS1_96.Verbatim, RS1_97.Verbatim, MRP_SURVEY_DP_STACK_ID, Zip.Postal.code, 
# Departure.flight...Travel.Dates, Arrival.flight...Travel.Dates, X, What.food.Beverages.want.to.find, 
# Regional.art..culture..or.historical.displays, Robots..tablet.interfacing..or.other.new.technology, weight
nombre = names(jdp) %in% c("airport","state", "RS1_96.Verbatim", "RS1_97.Verbatim", "MRP_SURVEY_DP_STACK_ID", "Zip.Postal.code", 
                           "Departure.flight...Travel.Dates", "Arrival.flight...Travel.Dates", "X", "What.food.Beverages.want.to.find", 
                           "Regional.art..culture..or.historical.displays", "Robots..tablet.interfacing..or.other.new.technology", "weight") 
jdp = jdp[!nombre] 

# More cleaning ... 
summary(jdp) 

table(jdp$Took.transportation.to.the.gate) 
table(jdp$Clarity.of.signs.directions.inside.the.terminal) 

# Convert some ordinal variables to binary for interpretative ease. 
jdp$clarity.signs = ifelse(jdp$Clarity.of.signs.directions.inside.the.terminal < 7,"unclear","clear") 
jdp$Clarity.of.signs.directions.inside.the.terminal = NULL 

jdp$no.merch.srvcs = jdp$Didn.t.purchase.any.merchandise.services
jdp$Didn.t.purchase.any.merchandise.services = NULL 
jdp$Food.beverages.purchased=NULL
jdp$Books.Magazines=NULL
jdp$Clothing=NULL 
jdp$Sunglasses=NULL
jdp$Electronics=NULL
jdp$Toiletries=NULL
jdp$Other.merchandise=NULL 
jdp$Other.services=NULL 
#jdp$merch = ifelse(jdp$Food.beverages.purchased==1, 1, ifelse(jdp$Books.Magazines==1,1,
#                                                              ifelse(jdp$Clothing==1,1, 
#                                                                     ifelse(jdp$Sunglasses==1,1,
#                                                                            ifelse(jdp$Electronics==1,1,
#                                                                                   ifelse(jdp$Toiletries==1,1,
#                                                                                         ifelse(jdp$Other.merchandise==1,1,0)))))))


# drop the 99s - this drops the population from 40k to 16k. (Try using a loop later) 
names(jdp) 
jdp = subset(jdp,jdp$Took.transportation.to.the.gate!=99 & #jdp$Clarity.of.signs.directions.inside.the.terminal!=99 & 
               jdp$Cleanliness.of.terminal.concourses.and.hallways!=99 & 
               jdp$Comfort.in.airport..e.g...seating..roominess..etc..!=99 & 
               jdp$Availability.of.activity.entertainment.options.in.the.airport!=99 & 
               jdp$Variety.of.food..beverage..and.retail.options!=99 & 
               jdp$Cleanliness.of.terminal.restrooms!=99 & 
               jdp$The.signage.directions.were.clear.easy.to.understand!=99 & 
               jdp$There.were.enough.signs.directions.throughout.the.terminal!=99 & 
               jdp$Recent.completed.renovations.or.new.building.s.!=99 & 
               jdp$I.was.able.to.clearly.hear.and.understand.the.announcements.within.the.gate!=99 & 
               jdp$The.gate.area.was.clean!=99 & 
               jdp$There.were.enough.seats.at.the.gate!=99 & 
               jdp$The.gate.area.was.comfortable!=99 & 
               jdp$There.were.enough.electrical.outlets.for.charging.phones.laptops!=99 & 
               jdp$The.gate.area.was.worn.out.or.outdated!=99) 

jdp$Terminal.Facility.Index=NULL 
cor(jdp$Overall.Satisfaction.Index, jdp$How.would.you.rate.your.overall.experience.at.Airport.Evaluated.) 
samp = sample_n(jdp, 1000, replace = FALSE)
plot(samp$Overall.Satisfaction.Index, samp$How.would.you.rate.your.overall.experience.at.Airport.Evaluated.) 

par(mfrow=c(1,2))
plot(jdp$How.would.you.rate.your.overall.experience.at.Airport.Evaluated., jdp$Overall.Satisfaction.Index) 
plot(jdp$Overall.terminal.facilities.experience, jdp$Overall.Satisfaction.Index) 
cor(jdp$How.would.you.rate.your.overall.experience.at.Airport.Evaluated., jdp$Overall.Satisfaction.Index) 
cor(jdp$Overall.terminal.facilities.experience, jdp$Overall.Satisfaction.Index) 
dev.off() 

jdp$Overall.terminal.facilities.experience=NULL 
jdp$How.would.you.rate.your.overall.experience.at.Airport.Evaluated.=NULL 

### Trees 
# If assuming a non-linear relationship between satisfaction and predictors. 
#http://www.di.fc.ul.pt/~jpn/r/tree/tree.html#classification-trees

#2. Regression tree 
  #2a. Grow a tree 
  #2b. Prune to get a sequence of subtrees. Use cross-validation to chose an alpha ... 
  # ... and pick the subtree corresponding to that alpha. 
set.seed (111) 
train = sample(1:nrow(jdp), nrow(jdp)/2) 
tree.jdp = tree(Overall.Satisfaction.Index~.-high, jdp, subset=train) 
length(names(jdp)) 
summary(tree.jdp) 
# Four variables (of 27) used in constructing a tree with eight nodes. 
# One or more of those four variables is active within the tree: 
plot(tree.jdp) 
text(tree.jdp, pretty=0)
# The label indicates the left branch at a split. 
# So those experiencing lower "comfort" levels at the terminal (below 7.5) 
# also have lower total satisfaction. 
# Of those with worse terminal-specific experiences, those in "OSAT Zone Satisfaction" groups (below 1.5) 
# have even worse experiences than others. 
# And of those with better terminal-specific experiences, access to entertainment or "activity" was a strong predictor 
# in total satisfaction. 
# Further, terminal cleanliness contributed to higher satisfaction levels, although 
# pruning the tree to five or six nodes eliminates cleanliness: 
cv.jdp = cv.tree(tree.jdp) 
cv.jdp # Eight-node tree provides the lowest error 
plot(cv.jdp$size, cv.jdp$dev ,type='b') 
prune.jdp = prune.tree(tree.jdp, best = 5) 
plot(prune.jdp) 
text(prune.jdp, pretty=0) 


# Split training test for test error estimation 
set.seed(113)   
smp_siz = floor(0.75*nrow(jdp)) 
train_ind = sample(seq_len(nrow(jdp)),size = smp_siz) 
train = jdp[train_ind,] 
test = jdp[-train_ind,] 

# In keeping with the cross-validation results, we use the unpruned tree to make predictions on the test set.

yhat = predict(tree.jdp, newdata=jdp[-train ,]) 
jdp.test = jdp[-train ,"Overall.Satisfaction.Index"] 
plot(yhat, jdp.test) 
abline(0, 1) 
mean((yhat-jdp.test)^2) 
sqrt(mean((yhat-jdp.test)^2)) 
# In other words, the test set MSE associated with the regression tree 
# is 5,500. The square root of the MSE is therefore around 74, 
# indicating that this model leads to test predictions that are within around 74 
# satisfaction points of the true level of satisfaction for the group.



# HERE 


# 
tree.jdp = tree(Overall.Satisfaction.Index ~ .-high, data=jdp) 
plot(tree.jdp) 
text(tree.jdp, cex=.6) 

summary(tree.jdp) 



# Classification 
jdp$high = ifelse(jdp$Overall.Satisfaction.Index>900,"high","low") 
# tree.jdp.class = tree(high ~ .-Overall.Satisfaction.Index, data=train) 
tree.jdp.class = rpart(high ~ .-Overall.Satisfaction.Index, data=train)
summary(tree.jdp.class) 
plot(tree.jdp.class, uniform=TRUE, branch=0.6, margin=0.05) 
text(tree.jdp.class, all=TRUE, use.n=TRUE) 
title("Training Set Classification Tree") 

# HERE 
predictions = predict(tree.jdp.class, test, type="class")
table(test$high, predictions) 

prune.class = prune(tree.jdp.class, cp=0.02) # pruning the tree 
plot(prune.class, uniform=TRUE, branch=0.6) 
text(prune.class, all=TRUE, use.n=TRUE) 

rpart.tree = rpart(high ~ ., data=train, parms = list(loss = lmat))  
predictions = predict(rpart.tree, test.set, type="class") 
table(test.set$Species, predictions)

plot(rpart.tree)
text(rpart.tree)

## Define a plotting function with decent defaults
plot.rpart.obj <- function(rpart.obj, font.size = 0.8) {
  ## plot decision tree
  plot(rpart.obj,
       uniform   = T,    # if 'TRUE', uniform vertical spacing of the nodes is used
       branch    = 1,    # controls the shape of the branches from parent to child node
       compress  = F,    # if 'FALSE', the leaf nodes will be at the horizontal plot
       nspace    = 0.1,
       margin    = 0.1, # an extra fraction of white space to leave around the borders
       minbranch = 0.3)  # set the minimum length for a branch
  
  ## Add text
  text(x      = rpart.obj,   #
       splits = T,           # If tree are labeled with the criterion for the split
       all    = T,           # If 'TRUE', all nodes are labeled, otherwise just terminal nodes
       use.n  = T,           # Use numbers to annotate
       cex    = font.size)   # Font size
}

plot.rpart.obj(rpart.tree, 1)

library(partykit)
rparty.tree <- as.party(rpart.tree)
rparty.tree

plot(rparty.tree)

fit <- rpart(Mileage~Price + Country + Reliability + Type, method="anova", data=cu.summary)
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results

summary(fit) # detailed summary of splits

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit) # visualize cross-validation results 

par(mfrow=c(1,1)) 

# plot tree
plot(fit, uniform=TRUE, main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8) 

# create attractive postcript plot of tree
post(fit, file = "c:/tree2.ps", title = "Regression Tree for Mileage ")


