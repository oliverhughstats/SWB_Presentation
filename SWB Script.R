
#install.packages("psych")
#install.packages("caTools")
# install.packages("ROCR")    

library(psych)
library(caTools)
library(ROCR)


## Import CSV File into RStudio

Data <- read.csv("C:\\Users\\oliver.hugh\\OneDrive - Perinatal Institute\\SWB\\SWB Dataset.csv")


## Question 1 - Summary of information in table

describe(Data$birthweight)
prop.table(table(Data$smoking))
prop.table(table(Data$sb))

## Question 2 - Outliers ?

boxplot(Data$birthweight)

Data1<-Data[Data$birthweight<10000 ,]

boxplot(Data1$birthweight)

Data2<-Data1[Data1$birthweight>0 ,]

boxplot(Data2$birthweight)

describe(Data$birthweight)
describe(Data2$birthweight)

## Question 3 - Relationship between low birthweight, smoking and stillbirth

Data2$lbw=ifelse(Data2$birthweight<2500,1,0)
prop.table(table(Data2$lbw))

log1 <-glm(sb~lbw, data=Data2, family="binomial")
exp(cbind(Odds_Ratio = coef(log1), confint(log1)))

log2 <-glm(sb~smoking, data=Data2, family="binomial")
exp(cbind(Odds_Ratio = coef(log2), confint(log2)))

log3 <-glm(sb~smoking + lbw, data=Data2, family="binomial")
exp(cbind(Odds_Ratio = coef(log3), confint(log3)))

