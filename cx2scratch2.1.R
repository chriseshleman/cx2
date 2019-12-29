
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
#install.packages("rsample") 
library(rsample)     # data splitting 
library(ipred)       # bagging
library(caret)       # bagging

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
#asq193 = as.data.frame(read_excel("./ACI - ASQ/2019 Q3/ACI ASQ Survey Main_ Q3 2019 Data-EXCEL-v1.xlsx"))
#names(asq193) = tolower(names(asq193)) 

jdp = read.csv("./JDPower18_noNA.csv") 
jdp.index = jdp$X 
jdp$X = NULL 
# Classify with a natural break. 

summary(jdp$Overall.Satisfaction.Index) 
boxplot(jdp$Overall.Satisfaction.Index) 
ggplot(jdp, aes(Overall.Satisfaction.Index)) +
  geom_density()
#hi = getJenksBreaks(jdp$Overall.Satisfaction.Index, 1, subset = NULL)
classIntervals(jdp$Overall.Satisfaction.Index, n = 2, style = "kmeans")
classIntervals(jdp$Overall.Satisfaction.Index, n = 2, style = "fisher") # jenks substitute for larger data sets 
# both kmeans and fisher (jenks substitute) give us around 730 as a natural breaking point. 

jdp$high = ifelse(jdp$Overall.Satisfaction.Index > 730, "high", "low") 

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
jdp$OSAT.Zones.of.Satisfaction=NULL #I just don't know what this is

### Trees 
# If assuming a non-linear relationship between satisfaction and predictors. 
#http://www.di.fc.ul.pt/~jpn/r/tree/tree.html#classification-trees

#1.5 Exploration 
ggplot(jdp, aes(Overall.Satisfaction.Index,Comfort.in.airport..e.g...seating..roominess..etc..)) +
  geom_point(size = 1) + #, alpha = .05
  geom_smooth() #method="loess" #method="lm" 

jdp$bath = ifelse(jdp$Cleanliness.of.terminal.restrooms>7,"clean_bathroom","nasty")
ggplot(jdp, aes(Overall.Satisfaction.Index,Comfort.in.airport..e.g...seating..roominess..etc..,color=jdp$Cleanliness.of.terminal.restrooms)) + #bath
  geom_point(size = 1) + #, alpha = .05
  geom_smooth() #method="loess" #method="lm" 

#2. Regression tree 
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
head(jdp_training) 
names(jdp_training) 
tree.train = tree(Overall.Satisfaction.Index~., jdp_training) 
summary(tree.train) 
plot(tree.train) 
text(tree.train, pretty=0, cex=0.75) 

# Cross-validation 
cv.train = cv.tree(tree.train) 
options(scipen=999)
plot(cv.train$size, cv.train$dev, type="b") 
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
mean((predict.jdp-jdp_test$Overall.Satisfaction.Index)^2) 

###################
### ABOVE IS 12/29 REDO OF INITIAL ISL CODE 
###################
# regression tree 
r.tree = rpart(
  formula = Overall.Satisfaction.Index~.,
  data    = jdp_training, #_training
  method  = "anova"#, 
  #control = list(cp = 0.025)
) 
pdf("./r_tree.pdf") 
r_tree = rpart.plot(r.tree, cex=0.6) # main="Tree (or whatever)" 
r_tree 
ggsave(r_tree, "./r_tree.pdf", device="pdf", width = 6, height = 4) 
dev.off() 
# the tree is doing some tuning automatically 
r.tree$cptable


# rpart automatically performed cross-validation (on the training set) ... 
# ... so redo but go bigger. 
r.tree.unconstrained = rpart(
  formula = Overall.Satisfaction.Index~.,
  data    = jdp_training,
  method  = "anova", 
  #minsplit=2000,
  minbucket=50, 
  control = list(cp = 0.001)
) 
#pdf("./r_tree.pdf") 
r_tree_unc = rpart.plot(r.tree.unconstrained, cex=0.6) # main="Tree (or whatever)" 
r_tree_unc 
ggsave(r_tree_unc, "./r_tree_unconstrained.pdf", device="pdf", width = 6, height = 4) 

jpeg("./r_tree_unc_selex.jpg") 
plotcp(r.tree.unconstrained)
abline(v = 6, lty = "dashed")
r.tree.unconstrained$cptable
# ... it appears error continues to fall as more splits are added. 
# why did rpart settle on 6 nodes instead of allowing the tree to grow (and the error to fall)? 
# if xerror is cross-validated error, that error continues to fall. Of course, this is ... the test error, I assume ... . 

#2c. Tuning 
# The function is already performing some tuning. 
# (The cost complexity (alpha) parameter is in action.)
# We can do more. 
#  In addition to the cost complexity (
#  α
#  ) parameter, it is also common to tune:
#  
#  minsplit: the minimum number of data points required to attempt a split before it is forced to create a terminal node. The default is 20. Making this smaller allows for terminal nodes that may contain only a handful of observations to create the predicted value.
#  maxdepth: the maximum number of internal nodes between the root node and the terminal nodes. The default is 30, which is quite liberal and allows for fairly large trees to be built.
