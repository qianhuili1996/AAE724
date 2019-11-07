
#title: "Data Preparation & Summary Stats"
#author: "Qianhui Li"

#-----------------------------------------------------

setwd("/Users/qianhuili/Desktop/GitHub/AAE724/Script/Data_cleaning")

library(tidyr)   
library(dplyr)   
library(leaps)
library(glmnet)
library(ggplot2)
library(gmodels)
library(MASS)
library(corrplot)
library(ISLR)
library(tree)
library(gridExtra)
library(ROCR)
library(rpart)
library(rpart.plot)
library(rattle)
library(pROC)
library(corrplot)
library(lfe)
library(car)
library(tidyverse)
library(viridis)
library(RColorBrewer)
library(ggpubr)
library(wesanderson)
library(plotly)
library(corrplot)
library(ROSE)
library(naniar)
library(caret)
library(blorr)
library(pROC)
#=============================================

##Data Preparation

bankoriginal<-read.csv("bank_data.csv",header=TRUE, sep=";", na.strings=c("unknown","non-existent"))

#----------
#Check # & % of missing values
gg_miss_var(bankoriginal)
gg_miss_var(bankoriginal, show_pct = TRUE)


#Since there is "999" in pdays means client was not previously contacted, I convert pdays into a dummy variable, never contacted(999)=0,others=1.

bankoriginal$pdays <-as.factor(bankoriginal$pdays)
bankoriginal$pdays <-ifelse(bankoriginal$pdays==999,0,1)


#The first variable that has the largest proportion of missing values is "default",
#However, it may be possible that customer is not willing to disclose this information to the banking representative. 
#Hence the unknown value in 'default' is actually a separate value.
#Thus I kept the variable "default", and I think it also make sense for "loan" and "housing" loan variable
bankoriginal$default <- as.character(bankoriginal$default)
bankoriginal$default[is.na(bankoriginal$default)] <- "refuse2disclose"

bankoriginal$loan<-as.character(bankoriginal$loan)
bankoriginal$loan[is.na(bankoriginal$loan)] <- "refuse2disclose"

bankoriginal$housing<-as.character(bankoriginal$housing)
bankoriginal$housing[is.na(bankoriginal$housing)] <- "refuse2disclose"
#As indicated by the data contributor, the duration is not known before a call is performed. 
#Also, after the end of the call y is obviously known. 
#Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.
#Thus I removed "duration"
bankoriginal = bankoriginal %>% 
  select(-duration)

#check for missing value graph again
gg_miss_var(bankoriginal)
gg_miss_var(bankoriginal, show_pct = TRUE)

#omit missing values
bank<-na.omit(bankoriginal)
sum(is.na(bank))

#Data summary
summary(bank)


#convert variable types
sapply(bank,class)

  #numerical variables
bank$age <- as.numeric(bank$age)
bank$campaign <- as.numeric(bank$campaign)
bank$previous <- as.numeric(bank$previous)
bank$emp.var.rate <- as.numeric(bank$emp.var.rate)
bank$cons.price.idx <- as.numeric(bank$cons.price.idx)
bank$cons.conf.idx <- as.numeric(bank$cons.conf.idx)
bank$euribor3m <- as.numeric(bank$euribor3m)
bank$nr.employed <- as.numeric(bank$nr.employed)

  #categorical variables
bank$job <-as.factor(bank$job)
bank$marital <-as.factor(bank$marital)
bank$education <-as.factor(bank$education)
bank$default <-as.factor(bank$default)
bank$loan <-as.factor(bank$loan)
bank$housing<-as.factor(bank$housing)
bank$contact <-as.factor(bank$contact)
bank$poutcome <-as.factor(bank$poutcome)
bank$day_of_week <-as.factor(bank$day_of_week)
bank$month <-as.factor(bank$month)

bank$y<-ifelse(bank$y =='yes',1,0)
bank$y <-as.factor(bank$y)

#---------------------

#Check for outliers for numerical variables
p1_age <- ggplot(bank, aes(y, age)) + geom_boxplot(aes(fill = y))

p1_campaign <- ggplot(bank, aes(y, campaign)) + geom_boxplot(aes(fill = y))

p1_previous <- ggplot(bank, aes(y, previous)) + geom_boxplot(aes(fill = y))

p1_emp.var.rate <- ggplot(bank, aes(y, emp.var.rate)) + geom_boxplot(aes(fill = y))

p1_cons.price.idx <- ggplot(bank, aes(y, cons.price.idx)) + geom_boxplot(aes(fill = y))

p1_cons.conf.idx<- ggplot(bank, aes(y, cons.conf.idx)) + geom_boxplot(aes(fill = y))

p1_euribor3m<- ggplot(bank, aes(y, euribor3m)) + geom_boxplot(aes(fill = y))

p1_nr.employed<- ggplot(bank, aes(y, nr.employed)) + geom_boxplot(aes(fill = y))


a1 <- c(p1_age,p1_campaign)
ggarrange(p1_age,p1_campaign, 
          nrow = 1)

b1 <- c(p1_previous,p1_emp.var.rate,
       p1_cons.price.idx)
ggarrange(p1_previous,p1_emp.var.rate,
          p1_cons.price.idx, 
          nrow = 1)

g1 <- c(p1_cons.conf.idx,
       p1_euribor3m,p1_nr.employed)
ggarrange(p1_cons.conf.idx,p1_euribor3m,p1_nr.employed, 
          nrow = 1)

#age==38770
x <- bank$age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
hb <- H + qnt[2]
hb #remove>69.5
ab <- bank[which(bank$age<hb),]

#campaign==35982
x1 <- bank$campaign
qnt1 <- quantile(x1, probs=c(.25, .75), na.rm = T)
H1 <- 1.5 * IQR(x1, na.rm = T)
hb1<- H1 + qnt1[2]
hb1 #remove>6
ac <- bank[which(bank$campaign<hb1),]



#cons.conf.idx
x5 <- bank$cons.conf.idx 
qnt5 <- quantile(x5, probs=c(.25, .75), na.rm = T)
H5 <- 1.5 * IQR(x5, na.rm = T)
hb5<- H5 + qnt5[2]
hb5 #remove>-26.95

#From the boxplot for "previous", I decided to treat observations larger than 2 as outliers, thus remove them.

#Result after removing outliers in numerical variables(34,370obs with 20 variables)
bank <- bank[which(bank$age<hb & bank$campaign<hb1 & bank$previous<2 & bank$cons.conf.idx<hb5),]


#Check for outliers for categorical variables with more than 3 categories

pic_job1 <-ggplot(bank, aes(x=job)) + geom_histogram(aes(y=(..count..)), stat='count', fill="slate blue", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Job",
       x="Job", y="Counts")
pic_job1
#From the histogram, there is no obvious small number of counts for jobs

pic_edu1 <-ggplot(bank, aes(x=education)) + geom_histogram(aes(y=(..count..)), stat='count', fill="yellowgreen", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Education",
       x="Education Status", y="Counts")
pic_edu1


table(bank$education)
#From the histogram, there is one obvious tiny number of counts for "illterate"(16 observations)
#Thus I decided to drop obs with "illiterate"
bank <-bank[bank$education!="illiterate",,drop=FALSE]

#After removing outliers for both numerical and categorical variables, there are 34,354 obs with 20 variables.




#----------------------

#Check and adjust data imbalance
counts <- table(bank$y)
barplot(counts,col=c("royalblue3","tomato3"),legend = rownames(counts), main = "Term Deposit")
CrossTable(bank$y)
#From the graph and the table, we can see that the dataset is highly imbalanced
#Since most machine learning classification algorithms are sensitive to unbalance in the predictor classes.
#I decided to resample the data by Synthetic Minority Oversampling Technique (SMOTE),
#which is a popular algorithm to creates synthetic observations of the minority class

set.seed(88)

balanced_data <- ROSE(y~., data=bank,seed=1)$data
CrossTable(balanced_data$y)

counts1 <- table(balanced_data$y)
barplot(counts1,col=c("royalblue3","tomato3"),legend = rownames(counts), main = "Customers' Responses")

#Hence, we now have 17245(50.2%) "no" responses, and 17109 (49.8%)"yes" responses, thus the data is balanced.

#=============================================

##Summary Statistics
summary(bank.dummies)

  #categorical variables exploration
pic_job <-ggplot(balanced_data, aes(x=job)) + geom_histogram(aes(y=(..count..)), stat='count', fill="slate blue", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Job",
       x="Job", y="Counts")
pic_job

  #The graph shows that the there are alot of customers work in administritive sector, and the least as entrepreneur.

aa <-ggplot(balanced_data, aes(x = job , fill = y)) +
  geom_bar(stat='count', position='dodge')
aa

  #The graph shows that there are customers that are admin, retired, or technicial are more willing to accept the offer.
#\\\\\\
pic_marital <-ggplot(balanced_data, aes(x=marital)) + geom_histogram(aes(y=(..count..)), stat='count', fill="light pink", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Marital",
       x="Marital Status", y="Counts")
pic_marital


bb<-ggplot(balanced_data, aes(x = marital , fill = y)) +
  geom_bar(stat='count', position='dodge')
bb
#\\\\\\

pic_edu <-ggplot(balanced_data, aes(x=education)) + geom_histogram(aes(y=(..count..)), stat='count', fill="yellowgreen", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Education",
       x="Education Status", y="Counts")
pic_edu


cc<-ggplot(balanced_data, aes(x = education , fill = y)) +
  geom_bar(stat='count', position='dodge')
cc
#\\\\\\

pic_default <-ggplot(balanced_data, aes(x=default)) + geom_histogram(aes(y=(..count..)), stat='count', fill="light blue", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Default",
       x="Default Status", y="Counts")
pic_default


dd<-ggplot(balanced_data, aes(x = default , fill = y)) +
  geom_bar(stat='count', position='dodge')
dd
#\\\\\\

pic_loan <-ggplot(balanced_data, aes(x=loan)) + geom_histogram(aes(y=(..count..)), stat='count', fill="orange1", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Loan",
       x="Loan Status", y="Counts")
pic_loan


ee<-ggplot(balanced_data, aes(x = loan , fill = y)) +
  geom_bar(stat='count', position='dodge')
ee
#\\\\\\
pic_housing <-ggplot(balanced_data, aes(x=housing)) + geom_histogram(aes(y=(..count..)), stat='count', fill="grey69", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Housing",
       x="Housing Status", y="Counts")
pic_housing


ff<-ggplot(balanced_data, aes(x = housing , fill = y)) +
  geom_bar(stat='count', position='dodge')
ff
#\\\\\\
pic_contact <-ggplot(balanced_data, aes(x=contact)) + geom_histogram(aes(y=(..count..)), stat='count', fill="firebrick", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Contact",
       x="Contact Approach", y="Counts")
pic_contact


gg<-ggplot(balanced_data, aes(x = contact , fill = y)) +
  geom_bar(stat='count', position='dodge')
gg
#\\\\\\
pic_poutcome <-ggplot(balanced_data, aes(x=poutcome)) + geom_histogram(aes(y=(..count..)), stat='count', fill="yellow1", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "poutcome",
       x="Previous Outcome", y="Counts")
pic_poutcome


hh<-ggplot(balanced_data, aes(x = poutcome , fill = y)) +
  geom_bar(stat='count', position='dodge')
hh
#\\\\\\
pic_dow <-ggplot(balanced_data, aes(x=day_of_week)) + geom_histogram(aes(y=(..count..)), stat='count', fill="turquoise4", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Day of Week",
       x="Day of Week", y="Counts")
pic_dow


jj<-ggplot(balanced_data, aes(x = day_of_week , fill = y)) +
  geom_bar(stat='count', position='dodge')
jj
#\\\\\\
pic_month <-ggplot(balanced_data, aes(x=month)) + geom_histogram(aes(y=(..count..)), stat='count', fill="darkseagreen4", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Month",
       x="Months", y="Counts")
pic_month


kk<-ggplot(balanced_data, aes(x = month , fill = y)) +
  geom_bar(stat='count', position='dodge')
kk
#\\\\\\
#response variable
pic_y <-ggplot(balanced_data, aes(x=y)) + geom_histogram(aes(y=(..count..)), stat='count', fill="red", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Subscribe or not",
       x="Subscription", y="Counts")
pic_y


CrossTable(balanced_data$y)



  #numerical variables exploration
p_age <- ggplot(balanced_data, aes(y, age)) + geom_boxplot(aes(fill = y))
hist(balanced_data$age, col = "yellow2", freq = FALSE)
abline(v = mean(balanced_data$age),
       col = "royalblue",
       lwd = 2)
abline(v = median(balanced_data$age),
       col = "light pink",
       lwd = 2)
legend(x = "topright", 
       c("Density plot", "Mean", "Median"),
       col = c("yellow2", "royalblue", "light pink"),
       lwd = c(2, 2, 2))
#The distribution shows that most customers oberved are less than 40 years old.


p_campaign <- ggplot(balanced_data, aes(y, campaign)) + geom_boxplot(aes(fill = y))

p_pdays <- ggplot(balanced_data, aes(y, pdays)) + geom_boxplot(aes(fill = y))
p_pdays

p_previous <- ggplot(balanced_data, aes(y, previous)) + geom_boxplot(aes(fill = y))
p_previous

p_emp.var.rate <- ggplot(balanced_data, aes(y, emp.var.rate)) + geom_boxplot(aes(fill = y))


p_cons.price.idx <- ggplot(balanced_data, aes(y, cons.price.idx)) + geom_boxplot(aes(fill = y))


p_cons.conf.idx<- ggplot(balanced_data, aes(y, cons.conf.idx)) + geom_boxplot(aes(fill = y))


p_euribor3m<- ggplot(balanced_data, aes(y, euribor3m)) + geom_boxplot(aes(fill = y))


p_nr.employed<- ggplot(balanced_data, aes(y, nr.employed)) + geom_boxplot(aes(fill = y))


a <- c(p_age,p_campaign,p_days)
ggarrange(p_age,p_campaign, 
          nrow = 1)

b <- c(p_previous,p_emp.var.rate,
       p_cons.price.idx)
ggarrange(p_previous,p_emp.var.rate,
          p_cons.price.idx, 
          nrow = 1)

g <- c(p_cons.conf.idx,
       p_euribor3m,p_nr.employed)
ggarrange(p_cons.conf.idx,p_euribor3m,p_nr.employed, 
          nrow = 1)

numericdata <- subset(balanced_data, select=c("age", "campaign","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed","pdays","previous"))

pairs(numericdata)

M <- cor(numericdata)
corrplot(M, method = "circle")
#or view in corr magnitudes
corrplot(M, method = "number")

#From the correlation plot, we can see that there are good correlations between 'cons.price.idx'&'emp.var.rate', 'cons.conf.idx'&'emp.var.rate',cons.conf.idx'&'cons.price.idx','cons.price.idx'&'nr.employed', cons.conf.idx'&'nr.employed','emp.var.rate'& nr.employed',nr.employed'& euribor3m.
 #Those multicollinearity problems may not affect our predictions but indeed affect causal inferences.
#=============================================
#Data Split

index <- createDataPartition(balanced_data$y, p = 0.5, list = FALSE)
train_data <- balanced_data[index, ]
test_data  <- balanced_data[-index, ]



#U??Use of one-hot-coding to transfer categorical variables into numerical variables????
dmy <- dummyVars(" ~ .", data = balanced_data)
bank.dummies<- data.frame(predict(dmy, newdata = balanced_data))
print(bank.dummies)


#===========================================================
#Regressions

## For logistic regression and neural nets, use 14 variables
modelk <- glm(y ~ ., data = balanced_data, family = binomial(link = 'logit'))
blr_step_aic_both(model)
modelk %>% blr_step_aic_both() %>%plot()

subsettrain<-c("y","nr.employed","month","poutcome","emp.var.rate","job","contact","cons.conf.idx","euribor3m","day_of_week","default","marital","education","housing","age")
train_lognn <-train_data[subsettrain]

subsettest<-c("y","nr.employed","month","poutcome","emp.var.rate","job","contact","cons.conf.idx","euribor3m","day_of_week","default","marital","education","housing","age")
test_lognn<-test_data[subsettest]
###logistic
logit_model <- glm(y ~.,family=binomial(link='logit'),data =train_lognn)
summary(logit_model)
anova(logit_model, test="Chisq")

  #confusion matrix for train
  log.pred.train <-predict(logit_model,data=train_lognn,type="response")
  log.pred1.train <-ifelse(log.pred.train>0.5,1,0)
  log.confusion.matrix.train <-table(log.pred1.train,train_lognn$y)
  log.confusion.matrix.train
  
  log.accuracy.train=sum(diag(log.confusion.matrix.train))/sum(log.confusion.matrix.train)
  log.accuracy.train
 

  
  #confusion matrix for test
  log.pred.test <-predict(logit_model,data=test_lognn,type="response")
  log.pred1.test <-ifelse(log.pred.test>0.5,1,0)
  error1 <-mean(log.pred1.test !=test_lognn$y)
  print(paste('Accuracy',1-error1))
  
 
  
 

###Decision tree
tree_model <- rpart(y ~ ., data = train_data,method="class")
tree_model
fancyRpartPlot(tree_model)

#predict train
predictions <- predict(tree_model, train_data, type = "class")

#confusion matrix train
tree.confusion.matrix.train <- prop.table(table(predictions, train_data$y))
tree.confusion.matrix.train

CrossTable(train_data$y, predictions,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual subscription status', 'predicted subscription status'))

  #train accuracy
tree.accuracy.train=sum(diag(tree.confusion.matrix.train))/sum(tree.confusion.matrix.train)
tree.accuracy.train

  
#predict test
cart_pred <- predict(tree_model , test_data,type="class")



  # Confusion matrix for test
tree.confusion.matrix.test <- prop.table(table(cart_pred, test_data$y))
tree.confusion.matrix.test

#test accuracy
tree.accuracy.test=sum(diag(tree.confusion.matrix.test))/sum(tree.confusion.matrix.test)
tree.accuracy.test

  # Cross table validation for test
CrossTable(test_data$y, cart_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual customers responses', 'predicted customers responses'))

##prune tree
set.seed(123)

printcp(tree_model)
plotcp(tree_model)

tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"]

bestcp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree_model, cp = bestcp)

fancyRpartPlot(tree.pruned)

# Compute the train accuracy of the pruned tree

train_data$pred <- predict(tree.pruned, train_data, type = "class")
accuracy_prun_train <- mean(train_data$pred == train_data$y)
accuracy_prun_train
pruned.confusion.matrix.train <- prop.table(table(train_data$pred, train_data$y))
pruned.confusion.matrix.train

# Compute the test accuracy of the pruned tree
test_data$pred <- predict(tree.pruned, test_data, type = "class")
accuracy_prune_test <- mean(test_data$pred== test_data$y)
accuracy_prune_test
pruned.confusion.matrix.test <- prop.table(table(test_data$pred, test_data$y))
pruned.confusion.matrix.test

#The tree after being pruned is the same as before


#=============================================================

#Random Forest
library(randomForest)
RF.model <- randomForest(y~., data=train_data, ntree=100, importance=TRUE)
RF.model
summary(RF.model)

#Next we display an error plot of the random forest model:
plot(RF.model)

RF.predict.train <- predict(RF.model, newdata = train_data)
RF.train.cm <- as.matrix(table(Actual1 = train_data$y, Predicted1 = RF.predict.train))
RF.train.cm
accuracy_train_rf=sum(diag(RF.train.cm))/sum(RF.train.cm)
accuracy_train_rf


library(knitr)
RF.predict <- predict(RF.model, newdata = test_data)
RF.cm <- as.matrix(table(Actual = test_data$y, Predicted = RF.predict))
RF.cm
kable(RF.cm, caption = "Random Forest Test Confusion Matrix")

accuracy_test_rf=sum(diag(RF.cm))/sum(RF.cm)
accuracy_test_rf

#Below we test the accuracy on the training and test datasets and we see that it is 90.87% and 83.45%, respectively. 
#The “out of sample” error is 16.51% and is in agreement with the OOB error:

#library(randomForestExplainer)
#explain_forest(RF.model, interactions = TRUE, data = train_data)



#=============================================


#==============================================================

#neural nets
library(nnet)
library(NeuralNetTools)
library(neuralnet)

set.seed(888)
nn <- train(y ~ .,
                  data = train_lognn,
                  method = "nnet")
print(nn)
plotnet(nn)

#train
nnpredtrain <- predict(nn, train_lognn)
resulttrainnn <-table(predicted=nnpredtrain,true=train_lognn$y)
resulttrainnn
acctrainnn =sum(diag(resulttrainnn))/sum(resulttrainnn)
acctrainnn

#test
nnpredtest <- predict(nn, test_lognn)
resulttestnn <-table(predicted=nnpredtest,true=test_lognn$y)
resulttestnn
acctestnn =sum(diag(resulttestnn))/sum(resulttestnn)
acctestnn

png("nn.png",height=2500, width=3000) 
plot(nn) 
dev.off()






#=============================================================

