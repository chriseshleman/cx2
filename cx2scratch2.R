
setwd("~/Dropbox/Work and research/Port Authority/cx2") 
library(readxl) 
library(mice) 
library(dplyr) 
library(Hmisc) 
library(imputeMissings) 
library(ggplot2) 
library(beepr) 
library(ggdendro) 
library(BAMMtools) 
#install.packages("classInt") 
library(classInt) 
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
library(ggplot2)

rm(list = ls()) # clear global environment 
cat("\014") # clear the console 
options(warn=-1) # suppress annoying warnings 

pmiss = function(x){sum(is.na(x))/length(x)*100} 


# What to do? This is Part 2, basic tree. Part 1 was an NA analysis and Part 3 can be advanced tree methods (bagging / random forest). 

#1. Load data and munge - classify 
#2. Tree JDP classification (and interpret) 
#3. Cross-validate and prune (and interpret) 
#4. Extension (bagging, random forest, boosting) (and interpret) 
#5. Repeat 2-5 with ASQ

###1. Load data and munge - classify 
asq193 = as.data.frame(read_excel("./ACI - ASQ/2019 Q3/ACI ASQ Survey Main_ Q3 2019 Data-EXCEL-v1.xlsx"))
names(asq193) = tolower(names(asq193)) 

jdp = read.csv("./JDPower18_noNA.csv") 
jdp.index = jdp$X 
jdp$X = NULL 
# Classify with a natural break. 

summary(jdp$Overall.Satisfaction.Index) 
boxplot(jdp$Overall.Satisfaction.Index) 
ggplot(jdp, aes(Overall.Satisfaction.Index)) +
  geom_density()
#hi = getJenksBreaks(jdp$Overall.Satisfaction.Index, 1, subset = NULL)
hi = classIntervals(jdp$Overall.Satisfaction.Index, n = 2, style = "kmeans")
hi
#jdp$high = ifelse(jdp$Overall.Satisfaction.Index > 900, "high", "low") 

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

# 3. Pruning 
# Further, terminal cleanliness contributed to higher satisfaction levels, although 
# pruning the tree to five or six nodes eliminates cleanliness: 
cv.jdp = cv.tree(tree.jdp) 
cv.jdp 
# The complex tree of eight nodes is still selected. 
# I could still prune it: 
plot(cv.jdp$size, cv.jdp$dev ,type='b') 
prune.jdp = prune.tree(tree.jdp, best = 5) 
plot(prune.jdp) 
text(prune.jdp, pretty=0) 

# ... But use the unpruned tree for prediction. 
yhat = predict(tree.jdp, newdata=jdp[-train ,]) 
jdp.test = jdp[-train ,"Overall.Satisfaction.Index"] 
plot(yhat, jdp.test) 
abline(0,1) 
mean((yhat-jdp.test)^2) 
sqrt(mean((yhat-jdp.test)^2)) 
# In other words, the test set MSE associated with the regression tree 
# is around 5,200. The square root of the MSE is therefore around 72, 
# indicating that this model leads to test predictions that are within around 72 
# satisfaction points of the true level of satisfaction for the group. 

#4. Bagging / random forest 
# ISL 
# Bagging is random forest where m=p. All predictors are considered at each tree split.
# Note that the random forest function only runs if characters are converted to factors. 
str(jdp) # "high" is a character and so is "clarity.signs" ... 
jdp.fac = jdp %>% mutate_if(is.character, as.factor) # ... convert everything (just these two characters) to factors. 
str(jdp.fac) 

set.seed(112) 
bag.jdp = randomForest(Overall.Satisfaction.Index ~.-high, data=jdp.fac, 
                       subset=train, mtry=27, importance =TRUE) 
bag.jdp 

yhat.bag = predict(bag.jdp, newdata=jdp.fac[-train ,]) 
plot(yhat.bag, jdp.test) 
abline(0,1) 
mean((yhat.bag-jdp.test)^2)
# So the test MSE associated with the bagged regression tree is 2667. 
# That's roughly half the MSE from the original tree. 
# Try re-doing by setting the number of trees manually ... 
bag.jdp2 = randomForest(Overall.Satisfaction.Index ~.-high, data=jdp.fac,
                        subset=train, mtry=27, ntree=25) 
yhat.bag2 = predict(bag.jdp2, newdata=jdp.fac[-train ,]) 
mean((yhat.bag2-jdp.test)^2)
# ... no difference, in fact the error grows slightly. 

# Try a random forest 
set.seed (113) 
rf.jdp = randomForest(Overall.Satisfaction.Index ~.-high, data=jdp.fac, 
                       subset=train, mtry=9, importance =TRUE) 
rf.jdp 

yhat.rf = predict(rf.jdp, newdata=jdp.fac[-train ,]) 
plot(yhat.rf, jdp.test) 
abline(0,1) 
mean((yhat.rf-jdp.test)^2) 

# Random forest provides the lowest test error. 
importance(rf.jdp) 
varImpPlot(rf.jdp, type=1) 
varImpPlot(rf.jdp, type=2) 
