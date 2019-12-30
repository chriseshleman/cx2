
setwd("~/Dropbox/Work and research/Port Authority/cx2") 
library(dplyr) 
library(ggplot2) 
library(beepr) 
library(tidyr) 
library(tidyverse) 
library(tree) 
library(randomForest) 
library(rpart) 
library(rpart.plot)
library(rsample) 
library(caret) 

rm(list = ls()) # clear global environment 
cat("\014") # clear the console 
options(warn=-1) # suppress annoying warnings 

pmiss = function(x){sum(is.na(x))/length(x)*100} 


# This is Part 2, trees (basic and advanced). Part 1 was an NA analysis. 

#1. Load data and munge - classify 
#2. Tree JDP (and interpret) 
#3. Cross-validate and prune (and interpret) 
#4. Extension (bagging, random forest, boosting) (and interpret) 

# Complete 1-4 above with discussions. 
# Catalog visualizations and save to folder as pdf or png. 
# Pick 2 key visualizations and toss the rest, and the summary outputs, into an appendix. 
# Visualization from page 311 and a variable importance plot from website (listed below) 

###1. Load data and munge. Predictor is almost continuous, so don't classify. 

jdp = read.csv("./JDPower18_noNA.csv") 
jdp.index = jdp$X 
jdp$X = NULL 

sat = ggplot(jdp, aes(Overall.Satisfaction.Index, fill="gray")) + 
  geom_density(alpha = 0.5) + 
  theme(legend.position = "none") + 
  labs(x = "Overall Satisfaction", y = "") 
ggsave(sat, filename = "./Satisfaction.png", width = 5, height = 4, dpi = 300, units = "in", device='png')
rm(sat) 
classIntervals(jdp$Overall.Satisfaction.Index, n = 2, style = "kmeans")
classIntervals(jdp$Overall.Satisfaction.Index, n = 2, style = "fisher") # jenks substitute for larger data sets 
# both kmeans and fisher (jenks substitute) give us around 730 as a natural breaking point. 
# but I won't classify - not sure what the analysis would gain aside from interpretability. 
# jdp$high = ifelse(jdp$Overall.Satisfaction.Index > 730, "high", "low") # if was to classify 

names(jdp) 
# Subtract airport, state, RS1_96.Verbatim, RS1_97.Verbatim, MRP_SURVEY_DP_STACK_ID, Zip.Postal.code, 
# Departure.flight...Travel.Dates, Arrival.flight...Travel.Dates, X, What.food.Beverages.want.to.find, 
# Regional.art..culture..or.historical.displays, Robots..tablet.interfacing..or.other.new.technology, weight
nombre = names(jdp) %in% c("airport","state", "RS1_96.Verbatim", "RS1_97.Verbatim", "MRP_SURVEY_DP_STACK_ID", "Zip.Postal.code", 
                           "Departure.flight...Travel.Dates", "Arrival.flight...Travel.Dates", "X", "What.food.Beverages.want.to.find", 
                           "Regional.art..culture..or.historical.displays", "Robots..tablet.interfacing..or.other.new.technology", "weight") 
jdp = jdp[!nombre] 

# More cleaning ... 
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
par(mfrow=c(1,1)) 

jdp$Overall.terminal.facilities.experience=NULL 
jdp$How.would.you.rate.your.overall.experience.at.Airport.Evaluated.=NULL 
jdp$OSAT.Zones.of.Satisfaction=NULL #I just don't know what this is

# Brief exploration 
ggplot(jdp, aes(Overall.Satisfaction.Index,Comfort.in.airport..e.g...seating..roominess..etc..)) +
  geom_point(size = 1) + #, alpha = .05
  geom_smooth() #method="loess" #method="lm" 

jdp$bath = ifelse(jdp$Cleanliness.of.terminal.restrooms>7,"clean_bathroom","nasty")
ggplot(jdp, aes(Overall.Satisfaction.Index,Comfort.in.airport..e.g...seating..roominess..etc..,color=jdp$Cleanliness.of.terminal.restrooms)) + #bath
  geom_point(size = 1) + #, alpha = .05
  geom_smooth() #method="loess" #method="lm" 

### 2. Trees 
# If assuming a non-linear relationship between satisfaction and predictors. 
#http://www.di.fc.ul.pt/~jpn/r/tree/tree.html#classification-trees

#2a. Grow a tree 
#2b. Prune to get a sequence of subtrees. Use cross-validation to chose an alpha ... 
# ... and pick the subtree corresponding to that alpha. 
#2c. 
# ISL ch. 8 and https://uc-r.github.io/regression_trees 

# Training and test data 
set.seed(12345)
jdp$high=NULL 
jdp_split = initial_split(jdp, prop = .7) 
jdp_training = training(jdp_split) 
jdp_test = testing(jdp_split) 

# Tree 
tree.train = tree(Overall.Satisfaction.Index~., jdp_training) 
summary(tree.train) 
pdf("./Tree.pdf", width=6, height=4.5) 
plot(tree.train) 
text(tree.train, pretty=0, cex=0.6) 
dev.off() 

### 3. Cross-validation 
cv.train = cv.tree(tree.train) 
options(scipen=999) 
pdf("./Tree_cross_validated.pdf", width=6, height=4.5) 
plot(cv.train$size, cv.train$dev, type="b") 
dev.off() 
# Three variables get used as predictors in the training tree, some of them more than once. 
# The most important variable that has the largest reduction in SSE is general comfort. 
# The top split assigns travelers reporting "comfort" of 7 or less to the left; they account for 38 percent of travelers. 
# Their predicted index score is 640. 
# Those reporting 8 or above "comfort" average around 882. 
# Those reporting high comfort can be further split into those with a perfect 10 comfort level (961) and 
# those with 8-9, for which restroom cleanliness was a significant predictor. 
# Of the 38% with weak "comfort" scores, roughly half were adequately pleased with the concourses and hallways and averaged 719 points, just below the average overall. 
# The other half might have been comfortable with seating availability but were frequently unsatisfied with activity and entertainment options in the airport as a whole. 
# A fraction (3%) were unsatisfied with comfort level, cleanliness and seating, averaging 413. 

# In many cases you prune the tree 
prune.train = prune.tree(tree.train, best=5) 
plot(prune.train) 
text(prune.train,pretty=0, cex=0.75) 

# But cross-validation did the trick, so use the unpruned tree 
predict.jdp = predict(tree.train,newdata=jdp_test) 
plot(predict.jdp,jdp_test$Overall.Satisfaction.Index) 
abline(0,1) 

### 4a. Bagging (check the mtry and ntree functions) 
jdp_training$clarity.signs = NULL 
jdp_training$bath = NULL 
bag.train = randomForest(Overall.Satisfaction.Index~., data=jdp_training, mtry=8) #, ntree=15
predict.bag = predict(bag.train,newdata=jdp_test) 

# Performance check 
mean((predict.jdp-jdp_test$Overall.Satisfaction.Index)^2) 
mean((predict.bag-jdp_test$Overall.Satisfaction.Index)^2) 

### 4b. Random forest 
# (Question - is it dumb to try and predict like this using this many ordinal variables as predictors?) 
rf.jdp = randomForest(Overall.Satisfaction.Index~., data=jdp_training, mtry=5, importance=T) 
predict.rf = predict(rf.jdp,newdata=jdp_test) 
mean((predict.rf-jdp_test$Overall.Satisfaction.Index)^2) 

### 4c. Variable importance, if there's time. 
importance(rf.jdp) 
varImpPlot(rf.jdp) 
varImpPlot(rf.jdp, type=2) 
# Consider something pretty like this https://topepo.github.io/caret/variable-importance.html 

# Question: what is a tree an obvious alternative to? 
# How might I expect that alternative to perform? 