#Configure Working Diretory

setwd("<File Path>") #OR Select Files Using Browse code
getwd()

#Read CSV File data

bH <- read.csv("<File Path:Loan processing Private company survey Data>")

str(bH)
summary(bH)

#import or Install Required Librarys

library(dplyr) #Data Manipulation 
library(rpart) #Recursive partitioning of classification, regression and survival tree
library(irr) #Inter rater reliability Calculation
library(caret) #classification, regression
library(rattle) #GUI for mining
library(rpart.plot) #support to plot rpart trees
library(RColorBrewer) #helps to create colorful graphs using premade color palettes 

#Data Preparation & Exploration
#Check Column names

names(bH) #110 Columns

#Case study requires only cus_employername; Need to group Target var basis cus_employername 

bH_Group <- bH[, c("cus_employername", "TARGET")] #Other way bH_Target <- select(bH, 9, 110)

#Check Missed values 

colSums(is.na(bH_Group)) #for TARGET Na is 6217, cus_employername Na is 0
bH_Group$TARGET <- as.factor(bH_Group$TARGET)

#Quick Check in Data Summary and Data Type

str(bH_Group) # Total : 29105
summary(bH_Group) #Unique : 26813

#Other similar Variable Combination

bH_Group1 <- bH[, c("cus_occupation", "TARGET")]
bH_Group1$TARGET <- as.factor(bH_Group1$TARGET)
summary(bH_Group1) #Unique : 18199

#Delete missing values in TARGET Variable

missing_index <- which(is.na(bH_Group$TARGET))
bH_Cleaned <- bH_Group[-missing_index,]

str(bH_Cleaned) #22888 Rows # unique :21088
summary(bH_Cleaned) # 0 NA's for Both Variables 

# Count of Rows in Missed value, Duplicate list has been Removed
# Formula = # bH_MissedDuplicate = (TotalObservation of bH_Group - Unique bH_Group) -
#                       (TotalObservation of bH_Cleaned - Unique bH_Cleaned)

#bH_Missed Duplicate   = (29105 - 26813) - (22888 - 21088)
#Result    : 492

#Total Duplicate : 22888 - 21088 = 1800 - 6 = 1794

#Decision Tree Modeling 
#Build Decision Tree

bH_Model <- rpart(TARGET~cus_employername,data=bH_Cleaned,method="class", 
                  control=rpart.control(cp=0.005,minsplit=2), parms = list(split="gini"))

#ploting the Tree

fancyRpartPlot(bH_Model)

options(scipen = 999)

#Rules Extraction

asRules(bH_Model)

#Result:
#Rule number: 3 [TARGET=1 cover=8692 (38%) prob=0.88]
#Rule number: 2 [TARGET=0 cover=14196 (62%) prob=0.19]

#Node 2 Has Target Zero with probability 81% (100 - 19)
#Node 3 Has Target One  with probability 88% 

#node Extraction
bH_Model$node <- bH_Model$where
head(bH_Model$node, 50)

#OR
bH_Cleaned$EmployerGroup <- bH_Model$where
View(bH_Cleaned$EmployerGroup)

## Fullfill Business Expectation ##
#code/procedure to create a dummy variable denoting which rows of the data correspond to 
#"Group 1" and which rows correspond to "Group 2"

#Both Result has 2 for BAD and 3 for Good c(2L,3L)

#Lets Separate each in one Group Group 1, Group 2

#Group 1 - Consisting of all employer names where the bad rate is high
#Group 2 -  Consisting of all the employer names where bad rate is low

bH_Cleaned$EmployerGroup <- ifelse(bH_Cleaned$EmployerGroup == 2, "Group 1", "Group 2")
head(bH_Cleaned)
tail(bH_Cleaned)
View(bH_Cleaned) #shows who's bad rate High and Whos Bad Rate Low

#Determine bad rate in each group

bH_Cleaned$EmployerGroup <- as.factor(bH_Cleaned$EmployerGroup)
str(bH_Cleaned$EmployerGroup)

summary(bH_Cleaned$EmployerGroup)
## Group 1 Group 2 
## 14196    8692 

table(bH_Cleaned$EmployerGroup, bH_Cleaned$TARGET) / (nrow(bH_Cleaned)) * 100

##                 0         1
## Group 1 50.179133 11.844635
## Group 2  4.674939 33.301293

## With above Output, 
## Group 1 Consisting of all employer names where the bad rate is high., That is 50%
## Group 2 Consisting of all the employer names where bad rate is low., That is 4.7%





