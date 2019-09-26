
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
#=============================================

##Data Preparation

bankoriginal<-read.csv("bank_data.csv",header=TRUE, sep=";", na.strings=c("unknown","non-existent","999"))
bank<-na.omit(bankoriginal)
sum(is.na(bank))

#As indicated by the data contributor, the duration is not known before a call is performed. 
#Also, after the end of the call y is obviously known. 
#Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.

bank = bank %>% 
  select(-duration)
summary(bank)


#convert variable types
sapply(bank,class)

  #numerical variables
bank$age <- as.numeric(bank$age)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays <- as.numeric(bank$pdays)
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



#=============================================

##Summary Statistics
summary(bank)

  #categorical variables exploration
pic_job <-ggplot(bank, aes(x=job)) + geom_histogram(aes(y=(..count..)), stat='count', fill="slate blue", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Job",
       x="Job", y="Counts")
pic_job

  #The graph shows that the there are alot of customers work in administritive sector, and the least as entrepreneur.

aa <-ggplot(bank, aes(x = job , fill = y)) +
  geom_bar(stat='count', position='dodge')
aa

  #The graph shows that there are customers that are admin, retired, or technicial are more willing to accept the offer.
#\\\\\\
pic_marital <-ggplot(bank, aes(x=marital)) + geom_histogram(aes(y=(..count..)), stat='count', fill="light pink", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Marital",
       x="Marital Status", y="Counts")
pic_marital


bb<-ggplot(bank, aes(x = marital , fill = y)) +
  geom_bar(stat='count', position='dodge')
bb
#\\\\\\

pic_edu <-ggplot(bank, aes(x=education)) + geom_histogram(aes(y=(..count..)), stat='count', fill="yellowgreen", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Education",
       x="Education Status", y="Counts")
pic_edu


cc<-ggplot(bank, aes(x = education , fill = y)) +
  geom_bar(stat='count', position='dodge')
cc
#\\\\\\

pic_default <-ggplot(bank, aes(x=default)) + geom_histogram(aes(y=(..count..)), stat='count', fill="light blue", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Default",
       x="Default Status", y="Counts")
pic_default


dd<-ggplot(bank, aes(x = default , fill = y)) +
  geom_bar(stat='count', position='dodge')
dd
#\\\\\\

pic_loan <-ggplot(bank, aes(x=loan)) + geom_histogram(aes(y=(..count..)), stat='count', fill="orange1", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Loan",
       x="Loan Status", y="Counts")
pic_loan


ee<-ggplot(bank, aes(x = loan , fill = y)) +
  geom_bar(stat='count', position='dodge')
ee
#\\\\\\
pic_housing <-ggplot(bank, aes(x=housing)) + geom_histogram(aes(y=(..count..)), stat='count', fill="grey69", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Housing",
       x="Housing Status", y="Counts")
pic_housing


ff<-ggplot(bank, aes(x = housing , fill = y)) +
  geom_bar(stat='count', position='dodge')
ff
#\\\\\\
pic_contact <-ggplot(bank, aes(x=contact)) + geom_histogram(aes(y=(..count..)), stat='count', fill="firebrick", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Contact",
       x="Contact Approach", y="Counts")
pic_contact


gg<-ggplot(bank, aes(x = contact , fill = y)) +
  geom_bar(stat='count', position='dodge')
gg
#\\\\\\
pic_poutcome <-ggplot(bank, aes(x=poutcome)) + geom_histogram(aes(y=(..count..)), stat='count', fill="yellow1", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "poutcome",
       x="Previous Outcome", y="Counts")
pic_poutcome


hh<-ggplot(bank, aes(x = poutcome , fill = y)) +
  geom_bar(stat='count', position='dodge')
hh
#\\\\\\
pic_dow <-ggplot(bank, aes(x=day_of_week)) + geom_histogram(aes(y=(..count..)), stat='count', fill="turquoise4", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Day of Week",
       x="Day of Week", y="Counts")
pic_dow


jj<-ggplot(bank, aes(x = day_of_week , fill = y)) +
  geom_bar(stat='count', position='dodge')
jj
#\\\\\\
pic_month <-ggplot(bank, aes(x=month)) + geom_histogram(aes(y=(..count..)), stat='count', fill="darkseagreen4", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Month",
       x="Months", y="Counts")
pic_month


kk<-ggplot(bank, aes(x = month , fill = y)) +
  geom_bar(stat='count', position='dodge')
kk
#\\\\\\
#response variable
pic_y <-ggplot(bank, aes(x=y)) + geom_histogram(aes(y=(..count..)), stat='count', fill="red", alpha=0.5) + theme_minimal() + 
  theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.text.x     = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y     = element_text(size=10)) +
  labs(title    = "Subscribe or not",
       x="Subscription", y="Counts")
pic_y


CrossTable(bank$y)



  #numerical variables exploration
p_age <- ggplot(bank, aes(y, age)) + geom_boxplot(aes(fill = y))
hist(bank$age, col = "yellow2", freq = FALSE)
abline(v = mean(bank$age),
       col = "royalblue",
       lwd = 2)
abline(v = median(bank$age),
       col = "light pink",
       lwd = 2)
legend(x = "topright", 
       c("Density plot", "Mean", "Median"),
       col = c("yellow2", "royalblue", "light pink"),
       lwd = c(2, 2, 2))
#The distribution shows that most customers oberved are less than 40 years old.


p_campaign <- ggplot(bank, aes(y, campaign)) + geom_boxplot(aes(fill = y))


p_pdays <- ggplot(bank, aes(y, pdays)) + geom_boxplot(aes(fill = y))


p_previous <- ggplot(bank, aes(y, previous)) + geom_boxplot(aes(fill = y))


p_emp.var.rate <- ggplot(bank, aes(y, emp.var.rate)) + geom_boxplot(aes(fill = y))


p_cons.price.idx <- ggplot(bank, aes(y, cons.price.idx)) + geom_boxplot(aes(fill = y))


p_cons.conf.idx<- ggplot(bank, aes(y, cons.conf.idx)) + geom_boxplot(aes(fill = y))


p_euribor3m<- ggplot(bank, aes(y, euribor3m)) + geom_boxplot(aes(fill = y))


p_nr.employed<- ggplot(bank, aes(y, nr.employed)) + geom_boxplot(aes(fill = y))


a <- c(p_age,p_campaign,p_pdays)
ggarrange(p_age,p_campaign,p_pdays, 
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

numericdata <- subset(bank, select=c("age", "campaign","pdays","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed"))

pairs(numericdata)

M <- cor(numericdata)
corrplot(M, method = "circle")
#or view in corr magnitudes
corrplot(M, method = "number")

#From the correlation plot, we can see that there are good correlations between 'cons.price.idx'&'emp.var.rate', 'cons.conf.idx'&'emp.var.rate',cons.conf.idx'&'cons.price.idx','cons.price.idx'&'nr.employed', cons.conf.idx'&'nr.employed','emp.var.rate'& nr.employed',nr.employed'& euribor3m.
 #Those multicollinearity problems may not affect our predictions but indeed affect causal inferences.
#=============================================

