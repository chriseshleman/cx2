# cx2 scratch 

# OK, what's the first goal? Is it to understand market segmentation? 
# Or is it to get some predictors of overall satisfaction? 

# And what're the baseline statistics (from the first record) for those data points? 

# Maybe do a quick cluster analysis of each set of predictors, independently. 
  # This can be step one. Run this past Aviation and use it to identify some potential predictors from each group. 
# Then pick a set of, say, 10 potential predictors and run them through a survival test. 
  # This can be step two. 

# Principal components analysis finds linear combinations that are uncorrelated and capture variance. 
# This tries to pare the set into important variables that summarize the variance acros the data. 
# Important for high-dimensional data. 

# There are two raw data files in the folders, one apiece for ASQ data (from 2019 Q3) and JD Power data (from 2019). Everything else is a PDF report or, in a couple of cases, Excel files that include synthesized tables or data dictionaries (not the raw data we’re hoping for). 
# This is as of the morning of 11/5. 
# We have gotten started with those two data files. 
# One big step for this and all future quantitative analysis of these two data sets is to start making all data sets, for every quarter (ASQ) or year (JD Power), available. 
# Preferably using identical data structure and file naming scheme. 

### 

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

rm(list = ls()) # clear global environment 
cat("\014") # clear the console 
# options(warn=-1) # suppress annoying warnings 


# ASQ data from ACI 
asq193 = read_excel("./ACI - ASQ/2019 Q3/ACI ASQ Survey Main_ Q3 2019 Data-EXCEL-v1.xlsx")
asq193 = as.data.frame(asq193) 
names(asq193) = tolower(names(asq193)) 


# JD Power data has more than 400 fields 
#jd19 = read_excel("./JD Power/2019 Study/Raw Data/19 Airport_W1W2W3W4_Client SPSS.xlsx")
jd18 = read.csv("./JD Power/2018 Study/Raw Data/Client_SPSS_File_W1W4 (1).csv")

# JD Power names are in separate file with different formatting 
jdnames = read_excel("./JD Power/2018 Study/Raw Data/2018 W1-W4 Data Dictionary.xlsx", skip = 1) 
jdnames = as.data.frame(jdnames) 
jdnames = jdnames[,c(1,3)] # Pick only the field names and labels. Labels will be important later.
jdnames = as.data.frame(gsub("[[:punct:]]", "", as.matrix(jdnames))) # Remove special characters.

# Remove spaces in names, transpose them, and take the labels only for short-term use.
jdnames$Label = gsub(" ",".",jdnames$Label) 
jdnames.t = t(jdnames) 
jdlabels = jdnames.t[2,] # Take the second row only (labels) 

# Combine JD Power labels with data. 
jdlabels = head(jdlabels,411) # Only the first 411 rows count 
names(jd18) = c(jdlabels) 

# I'm guessing "Overall.Satisfaction.Index" is the variable of interest. 
summary(jd18$Overall.Satisfaction.Index) 
# Is it scaled from 100-1000 or something? 

# Clean up. 
# Take a hint from https://uc-r.github.io/hc_clustering 
  # Remove variables that might add little value or be difficult to analyze. 
names(jd18) 
jd18.2 = jd18[,-(1:7)] 
#jd18.2.2 = jd18.2[,101:200] 
#jd18.2.3 = jd18.2[201:300]
#jd18.2.4 = jd18.2[301:398]
#str(jd18.2.2) 
#str(jd18.2.3) 
#str(jd18.2.4) 
jd18.2$Departure.flight..Travel.Dates = NULL 
jd18.2$Arrival.flight..Travel.Dates = NULL 
jd18.2$ZipPostal.code = NULL 
jd18.2$MRPSURVEYDPSTACKID = NULL 
jd18.2$AF7.Verbatim = NULL 
jd18.2$Why.public.transportation.not.used = NULL 
jd18.2$What.foodBeverages.want.to.find = NULL 
jd18.2$F15.Verbatim = NULL 
jd18.2$RS6.Verbatim = NULL 
jd18.2$TF2B.Verbatim = NULL 
jd18.2$YF2.Verbatim = NULL 
jd18.2$YF2.Verbatim.1 = NULL 
jd18.2$L7.Verbatim = NULL 
jd18.2$Reason.for.NPS.rating = NULL 
jd18.2$Other.9 = NULL 
jd18.2$FB2.Verbatim = NULL 
jd18.2$RS196.Verbatim = NULL 
jd18.2$RS197.Verbatim = NULL 
jd18.2$Regarding.the.cleanliness.of.the.terminal.why.did.you.provide.a.rating.of.MRKTF21B.for.MRKAIRPORT = NULL 

# Remove columns with all NAS
#mi_data = mi(jd18.3, seed = 335) # This way takes forever - longer than it should.
not_all_na = function(x) {!all(is.na(x))}
jd18.2 = jd18.2 %>% select_if(not_all_na) 

summary(jd18.2) 


###
# Take a sample - 500 rows and 25 variables for short term training. 
set.seed(10021) 
jd.sample = jd18.2[sample(nrow(jd18.2), 500), ] # 500 random rows 
#jd.sample = jd.sample[c(1,5:28)] # First and 5th-28th variables 
jd.sample = jd.sample[complete.cases(jd.sample), ] 
jd.sample = jd.sample %>% drop_na() 

### HERE 
# Challenge - include all remaining variables but find a way to remove ones with a high number of NAs? 

# Now remove observations with NAs or impute 
jd.sample = na.omit(jd.sample) 
jd18.3 = na.omit(jd18.2) 


### Clustering 
# Standardize variables 
jd.sample = scale(jd.sample) 

# Heirarchal clustering 
hc.complete = hclust(dist(jd.sample), method="complete") 
hc.average = hclust(dist(jd.sample), method="average") 
hc.single = hclust(dist(jd.sample), method="single") 

par(mfrow=c(1,3))
plot(hc.complete, main="Complete linkage", xlab="", ylab="") 
plot(hc.average, main="Average linkage", xlab="", ylab="") 
plot(hc.single, main="Single linkage", xlab="", ylab="") 

# 
ggdendrogram(hc.complete, rotate = TRUE, theme_dendro = FALSE) 


#############################################################################
jd.sample = jd18.3[sample(nrow(jd18.3), 50), ] 

not_all_na = function(x) {!all(is.na(x))}
jd.sample = jd.sample %>% select_if(not_all_na) 

jd.sample2 = na.omit(jd.sample) 
cormat = round(cor(jd.sample),2) 
head(cormat)

melted_cormat = melt(cormat) 
head(melted_cormat) 

beep(2) 
