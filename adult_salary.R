##-------ADULT SALARY ASSIGNMENT--------##

#-----DATA DICTIONARY------#

#??? age: the age of an individual ??? Integer greater than 0
#??? workclass: a general term to represent the employment status of an individual ??? Private, Self¬emp¬not¬inc, Self¬emp¬inc, Federal¬gov, Local¬gov, State¬gov, Without¬pay, Never¬worked. 
#??? fnlwgt: final weight. In other words, this is the number of people the census believes the entry represents. ??? Integer greater than 0
#??? education: the highest level of education achieved by an individual. ??? Bachelors, Some¬college, 11th, HS-grad, Prof¬school, Assoc¬acdm, Assoc¬voc, 9th, 7th¬8th, 12th, Masters, 1st¬4th, 10th, Doctorate, 5th¬6th, Preschool.
#??? education¬num: the highest level of education achieved in numerical form. ??? Integer greater than 0 
#??? marital¬status: marital status of an individual. Married¬civ¬spouse corresponds to a civilian spouse while Married¬AF¬spouse is a spouse in the Armed Forces. ??? Married¬civ¬spouse, Divorced, Never¬married, Separated, Widowed, Married¬spouse¬absent, Married¬AF¬spouse.
#??? occupation: the general type of occupation of an individual ??? Tech¬support, Craft¬repair, Other¬service, Sales, Exec¬managerial, Prof¬specialty, Handlers¬cleaners, Machine¬op¬inspct, Adm¬clerical, Farming¬fishing, Transport¬moving, Priv¬house¬serv, Protective¬serv, Armed¬Forces.
#??? relationship: represents what this individual is relative to others. For example an individual could be a Husband. Each entry only has one relationship attribute and is somewhat redundant with marital status. We might not make use of this attribute at all ??? Wife, Own¬child, Husband, Not¬in¬family, Other¬relative, Unmarried.
#??? race: Descriptions of an individual’s race ??? White, Asian¬Pac¬Islander, Amer¬Indian¬Eskimo, Other, Black. 
#??? sex: the biological sex of the individual ??? Male, Female 
#??? capital¬gain: capital gains for an individual ??? Integer greater than or equal to 0 
#??? capital¬loss: capital loss for an individual ??? Integer greater than or equal to 0 
#??? hours¬per¬week: the hours an individual has reported to work per week ??? continuous.


###Importing the dataset
adultsalary <- read.csv("adult_sal.csv", stringsAsFactors = F, check.names = F)

###structure of the dataset
str(adultsalary)
summary(adultsalary)


##packages
library(dplyr)
library(stringr)
library(rpart)
library(rpart.plot)
library(caret)
library(car)
library(caTools)
library(mice)
library(randomForest)
library(ggplot2)
library(gridExtra)
library(dummies)

##DATA CLEANSING
#NA's
colSums(is.na(adultsalary))  ##No NA's present.

#BLANKS
colSums(adultsalary=="")  ##no BLANKS


#cheking distinct values.
sapply(adultsalary, n_distinct)

#Duplicates
sum(duplicated(adultsalary[-1])) #no duplicates.
24/32561 #negligible duplicacy. Taking it as it is.

####
table(adultsalary$income)
prop.table(table(adultsalary$income))*100    ##only 24% earn >50k, 76% earn <=50k


####_____EDA_____####

##DATA VISUALISATION based on each variable.
#creating seperate functions for visualising categorical and continous variables.
cat_plot <- function(cat_var){
  p1 <- ggplot(adultsalary[1:32561,],aes(x = adultsalary[1:32561,cat_var], fill = as.factor(income))) + 
    geom_bar(col = "black") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = cat_var, y = 'income')
  p2 <- ggplot(adultsalary[1:32561,],aes(x = adultsalary[1:32561,cat_var], fill = as.factor(income))) + 
    geom_bar(col = "black",position = "fill") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = cat_var, y = "income")
  grid.arrange(p1, p2)}


cont_plot <- function(cont_var){
  p1 <- ggplot(adultsalary[1:32561,],aes(x = adultsalary[1:32561,cont_var], fill = as.factor(income))) + 
    geom_histogram(col = "black", bins = 10) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = cont_var, y = "income")
  p2 <- ggplot(adultsalary[1:32561,],aes(x = adultsalary[1:32561,cont_var], fill = as.factor(income)))+ 
    geom_histogram(col = "black",position = "fill", bins = 10) +theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = cont_var, y = "income")
  grid.arrange(p1, p2)}


#######UNIVARIATE/BIVARIATE ANALYSIS#######

### 1.AGE
table(adultsalary$age)

cont_plot("age")    ##Very few people earn over 50k under 21 years of age. Max people of age group 37-47 earn over 50K.
knitr::kable(table(adultsalary$age,adultsalary$income))
n_distinct(adultsalary$age)

plot(quantile(adultsalary$age, seq(0,1,0.01))) #outliers present.lets check.
quantile(adultsalary$age, seq(0,1,0.01))

#Outlier Treatment
adultsalary$age[adultsalary$age >74] <- 79
plot(quantile(adultsalary$age, seq(0,1,0.01))) #seems much better.


### 2.Type_Employer
n_distinct(adultsalary$type_employer)
table(adultsalary$type_employer)
adultsalary$type_employer[adultsalary$type_employer=="?"] <- "unknown"

cat_plot("type_employer")
knitr::kable(table(adultsalary$type_employer, adultsalary$income))  ## max people belonging to the self employed income category earns over 50K.
                                                                    #(ratio is considered)

### 3. Final Weight
n_distinct(adultsalary$fnlwgt)
cont_plot("fnlwgt")
summary(adultsalary$fnlwgt)


plot(quantile(adultsalary$fnlwgt, seq(0,1,0.01))) #outliers present.lets check.
quantile(adultsalary$fnlwgt, seq(0,1,0.01))

#Outlier Treatment
adultsalary$fnlwgt[adultsalary$fnlwgt >510072] <- 550000
plot(quantile(adultsalary$fnlwgt, seq(0,1,0.01))) #seems much better.

### 4. Education
n_distinct(adultsalary$education)
table(adultsalary$education)

cat_plot("education") ##clearly higher education outperform others.


### 5. Education Number
n_distinct(adultsalary$education_num)
table(adultsalary$education_num)

cont_plot("education_num")  ##Higher the number, Higher the Salary.

### 6. Marital Status
n_distinct(adultsalary$marital)
cat_plot("marital")       ##More people are married who earns over 50K.

### 7. Occupation
n_distinct(adultsalary$occupation)
table(adultsalary$occupation)

adultsalary$occupation[adultsalary$occupation=="?"] <- "unknown"

### 8. Relationship
n_distinct(adultsalary$relationship)
table(adultsalary$relationship)

cat_plot("relationship")  ##either husband or wife are more in no. i.e married people, who earns over 50K.

### 9.Race
n_distinct(adultsalary$race)
table(adultsalary$race)

cat_plot("race")   ##more people from asian region earns over 50K.

### 10. sex
n_distinct(adultsalary$sex)
table(adultsalary$sex)
knitr::kable(table(adultsalary$sex, adultsalary$income))
cat_plot("sex")  ##more no. of male candidates earns over 50K


### 11. Capital Gain
n_distinct(adultsalary$capital_gain)
table(adultsalary$capital_gain)

cont_plot("capital_gain")
plot(quantile(adultsalary$capital_gain, seq(0,1,0.01))) #outliers present.lets check.
quantile(adultsalary$capital_gain, seq(0,1,0.01))

#Outlier Treatment
adultsalary$capital_gain[adultsalary$capital_gain >15025] <- 20000
plot(quantile(adultsalary$capital_gain, seq(0,1,0.01))) #seems much better.

### 12. capital loss
n_distinct(adultsalary$capital_loss)
table(adultsalary$capital_loss)

cont_plot("capital_loss")

plot(quantile(adultsalary$capital_loss, seq(0,1,0.01)))
quantile(adultsalary$capital_loss, seq(0,1,0.01))

#leaving it as it is.

### 13. Hours per Week
n_distinct(adultsalary$hr_per_week)
cont_plot("hr_per_week")         ##max people working for 40 hrs a week earns over 50K.

knitr::kable(table(adultsalary$hr_per_week, adultsalary$income))

plot(quantile(adultsalary$hr_per_week, seq(0,1,0.01))) #outliers present.lets check.
quantile(adultsalary$hr_per_week, seq(0,1,0.01))

adultsalary$hr_per_week[adultsalary$hr_per_week >80] <- 87
plot(quantile(adultsalary$hr_per_week, seq(0,1,0.01))) #seems much better.


### 14. Country
n_distinct(adultsalary$country)
cat_plot("country")
##way too many categories. pattern detection very hard.
adultsalary$country <- NULL


### 15. Income
adultsalary$income[adultsalary$income==">50K"] <- "1"

adultsalary$income[adultsalary$income=="<=50K"] <- "0"
#target variable, leaving it as it is.


#Separating categorical and continous variables
colnames(adultsalary)
sapply(adultsalary, n_distinct)

cont_vars <- adultsalary[,c("age", "fnlwgt", "education_num","capital_gain","capital_loss",
                          "hr_per_week")]


# Check correlation among continuous variables
cormat <- cor(cont_vars)
require(corrplot)
corrplot(cormat, method = 'number')
#relatively less correlation between the variables.

# Scale continuous variables
cont_vars <- as.data.frame(sapply(cont_vars, scale))

# categorical variables
cat_vars <- adultsalary[, !colnames(adultsalary) %in% colnames(cont_vars)]
names(cat_vars)

summary(cat_vars)

# Converting to factor (as categorical)
cat_vars <- as.data.frame(sapply(cat_vars, as.factor))
summary(cat_vars)

sapply(cat_vars, n_distinct) ##1st column is primary key.

# Joing them to create master dataset again
master <- cbind(cat_vars[,1], cont_vars, cat_vars[,-1])

colnames(master)[1] <- "serial_no."

str(master)
# All variables in proper format.

# EDA Complete...

#### ---- Model Building ---- ####

prop.table( table(adultsalary$income)) * 100
table(adultsalary$income)
#target variable imbalanced.

require(DMwR)
set.seed(123)
master <- SMOTE(income ~ ., data = master, perc.over = 500, perc.under = 200)

prop.table( table(master$income)) * 100
table(master$income)

require(caTools)

set.seed(999)
index = sample.split(master$income, SplitRatio = 0.75)

train <- master[index, ]
test <- master[!index, ]

require(randomForest)

rf_model <- randomForest(income ~ ., data = train[,-1], ntree = 1000,
                         proximity = F, do.trace = T, importance = T)

predicted_probability <- predict(rf_model, test[,-c(1,15)], type = "prob")[,2]
summary(predicted_probability)

actual_1<- as.factor(test$income)

OUT <- matrix(nrow = 100, ncol = 3)
s <- seq(min(predicted_probability), max(predicted_probability), length.out = 100)

require(caret)

cutoff_finder <- function(cutoff) {
  pred_1 <- factor(ifelse(predicted_probability >= cutoff, "1", "0"))
  conf <- confusionMatrix(pred_1, actual_1, positive = "0")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- c(sens, spec, acc) 
  return(out) }

for(j in 1:100) {OUT[j,] <- cutoff_finder(s[j])}


cutoff <- s[which.min(abs(OUT[,1]-OUT[,2]))]
cutoff

# Thus, predicting final booking as per cutoff value of predicted probability
pred_1 <- factor(ifelse(predicted_probability >= cutoff, "1", "0"))

con_mat <- confusionMatrix(pred_1, actual_1, positive = "1")
con_mat

# Accuracy : 0.96;  Sensitivity : 0.9613;   Specificity : 0.9591
