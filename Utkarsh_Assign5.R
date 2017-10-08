#For removing scientific notations
options("scipen" =100,digits = 2)
options(warn=-1)

#Package 
library(caret)
library(ggplot2)
library(data.table)
library(rpart)
library(rattle)
library(rpart.plot)

dt <- read.csv("bank-additional-full.csv")
dim(dt)
str(dt)
summary(dt)

a=list()

#To find out the numerical features in the dataset
for(i in 1:ncol(dt)){
  a[i]=is.numeric(dt[,i])
}
col <- names(dt)[a==T]
col

#Mode Function
Mode <- function(data1,i)
{
  tab <- table(data1[,i])
  Mode <- names(tab)[tab==max(tab)]
  return(Mode)
}



#Univariate Analysis
#Age
hist(dt$age)
summary(dt$age)
boxplot(dt$age)
quantile(dt$age,seq(0,1,.05))
#Righ Skewed

#Job
table(dt$job)
#Missing Value:Unknown -Count-330

#Marital
table(dt$marital)
#Missing Value:Unknown - Count 80

#Education
table(dt$education)
#Missing Value:Unknown - Count 1731

#Default
table(dt$default)
prop.table(table(dt$default))
#Around 79% data has No
#Around 20% data are missing
#Just 3 observations for Yes
#Wont be a good feature to be used for prediction


#Housing
table(dt$housing)
#Missing Value:Unknown - Count 990

#Loan
table(dt$loan)
#Missing Value:Unknown - Count 990

#Contact
table(dt$contact)
#Clients were more appraached through cellular communication rather telephonic communication

#Month
table(dt$month)
#Most of the contacts were made in the month of May

#Day_of_Week
table(dt$day_of_week)
#Majority of the contacts were made on thursday

#Duration
dur_0=table(dt$duration[dt$duration==0])
paste("No of people who had call duration 0 are",dur_0)

a=table(dt$duration)
head(a)
b=max(a)
c=names(a)[a=max(a)]
paste("Maximum calls were made of duration",b,"and number of calls made were",c)


#Campaign
table(dt$campaign)
#Through Campaign one most of the clients were contacted

#pdays
prop.table(table(dt$pdays))
#Around 96% of data are missing.
#We will drop this variable

#Previous 
table(dt$previous)
#Most of the clients were not approached earlier for campaigns

#poutcome
table(dt$poutcome)
#Most of the results for earlier marketing campaign is non existent

#Emp.var.rate
table(dt$emp.var.rate)
#Majority of the people fall in employment variation rate category of 1.4


#Cons.price.index
table(dt$cons.price.idx)
#Majority of the people fall in consumer price index category of 92.893

#cons.conf.index
table(dt$cons.conf.idx)
#MAjority of the people fall in consumer confidence index category of -42.7

#euribor3m
table(dt$euribor3m)

#nr.employed
table(dt$nr.employed)

#Bivariate Analysis
#Age vs Y
ggplot(dt,aes(x=age))+geom_histogram(aes(fill=y),col="Black")+facet_grid(y~.,scales= c("free_y"))
#Pattern looks almost similar for successfula and unsuccessful subscription for different age group
ggplot(dt,aes(x=y,y=age))+geom_boxplot()
summary(dt$age[dt$y=="yes"])
summary(dt$age[dt$y=="no"])


#Job vs y
ggplot(dt,aes(x=job))+geom_bar(aes(fill=y))+facet_grid(y~.)
#overall the trend is similar for subcribed and non subscribed people except that 

#Education vs y
ggplot(dt,aes(x=education))+geom_bar(aes(fill=y))+facet_grid(y~.)
#Trend is almost similar for the people who opted for subscription and the ones who didnot and it
#is highest in education type University degree
#More missing values present in No category
table(dt$education,dt$y)

#Default vs y
ggplot(dt,aes(x=default))+geom_bar(aes(fill=y))+facet_grid(y~.)

#People whit no credit in default didinot subscribe to deposit

#Housing vs y

ggplot(dt,aes(x=housing))+geom_bar(aes(fill=y))+facet_grid(y~.)

#More successful subscription are coming from people who have home loan

#Contact vs y
ggplot(dt,aes(x=contact))+geom_bar(aes(fill=y))+facet_grid(y~.)

#Cellular contacts has made more successful subscription as compared to telephone

#Month vs y
ggplot(dt,aes(x=month))+geom_bar(aes(fill=y))+facet_grid(y~.,scales = "free_y")
prop.table(table(dt$y,dt$month),margin =2)

#Conversion rates has been poor in every month except march where successful conversion rate
#was around 50 %

#Day_of_week vs y
ggplot(dt,aes(x=day_of_week))+geom_bar(aes(fill=y))+facet_grid(y~.,scales = "free_y")
prop.table(table(dt$y,dt$day_of_week),margin =2)

#Among all the days,on thursday conversion rates were higher

#On thursday maximum number of successful subscriptions happened and 
#On monday maximum number of unsuccessful subscriptions happened

#Duration vs y
ggplot(dt[dt$duration<=1500,],aes(x=duration))+geom_histogram(aes(fill=y),col="Black")+facet_grid(y~.,scales = "free_y")

ggplot(dt,aes(x=y,y=duration))+geom_boxplot(aes(fill=y))

summary(dt$duration[dt$y=="yes"])
summary(dt$duration[dt$y=="no"])

#More successful subscriptions has happened when duration of call is high,however there are outliers as well

#Campaign vs y

ggplot(dt[dt$campaign<=25,],aes(x=campaign))+geom_histogram(aes(fill=y),col="Black")+facet_grid(y~.,scales = "free_y")
ggplot(dt,aes(x=y,y=campaign))+geom_boxplot(aes(fill=y))
summary(dt$campaign[dt$y=="yes"])
summary(dt$campaign[dt$y=="no"])

#Majority of the sucessful subscriber are the ones who were contacted once.
head(prop.table(table(factor(dt$campaign),dt$y),margin=2),5)

#More number of calls is not giving any successful conversions.
tail(prop.table(table(factor(dt$campaign),dt$y),margin=2),5)


#pdays
table(dt$y,dt$pdays)
#Percent of missing values
length(dt$pdays[dt$pdays==999])/nrow(dt)
#Around 96% of data are missing.We will drop this variable

#previous
table(dt$y,dt$previous)
prop.table(table(dt$y,dt$previous),margin = 1)
prop.table(table(dt$y,dt$previous),margin = 2)
ggplot(dt,aes(x=factor(previous)))+geom_bar(aes(fill=y))+facet_grid(y~.)

#Clients who were earlier contacted 5 times are the ones with maximum successful conversions
#Oon average as the number of earlier contacts increased to 5 the conversion rate increased. 
#However pattern is not montonicall increasing or decreasing ,so not much can be said

#poutcome
length(dt$poutcome[dt$poutcome=="nonexistent"])/nrow(dt)
ggplot(dt,aes(x=factor(poutcome)))+geom_bar(aes(fill=y))+facet_grid(y~.)
prop.table(table(dt$y,dt$poutcome),margin = 1)
prop.table(table(dt$y,dt$poutcome),margin = 2)

#For around 86% of data we dont have values,we will ignore this variable


#Social and economics attributes
#emp.var.rate
prop.table(table(dt$y,factor(dt$emp.var.rate)),2)
ggplot(dt,aes(x=emp.var.rate))+geom_histogram(aes(fill=y),col="Black")+facet_grid(.~y,scales = "free_y")
ggplot(dt,aes(x=y,y=emp.var.rate))+geom_boxplot(aes(fill=y))

#Considering as factor
ggplot(dt,aes(x=factor(emp.var.rate)))+geom_bar(aes(fill=y))+facet_grid(y~.)

#Proportion of people who opted for scheme is maximum for emp.var.rate=-1.7 as compared to who didnot opt for

# cons.price.idx: consumer price index - monthly indicator (numeric) 
# 18 - cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
# 19 - euribor3m: euribor 3 month rate - daily indicator (numeric)
# 20 - nr.employed: number of employees - quarterly indicator (numeric)

#cons.price.idx
prop.table(table(dt$y,factor(dt$cons.price.idx)),2)


#Missing Value Treatment:Use of Decision Tree for imputation
summary(dt)

#Features having missing values:
# 1.Job-unknown
# 2.Marital-unknown
# 3.Education-Unknown
# 4.Default-Unknown
# 5.Housing-unknown
# 6.Loan-Unknown


dt_transform <- dt

ggplot(dt,aes(x=age))+geom_histogram(aes(fill="job"))+facet_grid(job~.)
ggplot(dt,aes(x=education))+geom_bar(aes(fill="job"))+facet_grid(job~.)



#Decision tree


gtrain<-dt[dt$job!="unknown",]
gtest<-dt[dt$job=="unknown",]
depFit <- rpart(data=gtrain,job~.,xval=3)
fancyRpartPlot(depFit)

#Age and Education are important feature for determining job of people


#Predicitng missing values
pred_dep <-  predict(depFit,newdata=gtest,type="class")

#Accuracy
p<-predict(depFit,gtrain,type="class")
acc=sum(p==gtrain[,2])/length(p)
acc

#Using Mode and checking accuracy
job_Mode <- Mode(dt,2)
job_Mode
pre=gtrain$job 
pre <-  "admin."
acc=sum(pre==gtrain[,2])/length(p)
acc
#Accuracy of around 50% which is better than the mode value which gives 25% accuracy on train data


#impute missing job
dt_transform$job[dt_transform$job=="unknown"]<-predict(depFit,gtest,type="class")
summary(dt_transform$job)
#Dropping unknown factor
dt_transform$job <- factor(dt_transform$job)
summary(dt_transform$job)
#Unknown factor removed 


#Marital
gtrain<-dt[dt$marital!="unknown",]
gtest<-dt[dt$marital=="unknown",]
depFit <- rpart(data=gtrain,marital~.,xval=3)
fancyRpartPlot(depFit)

#Age and education seems to be important feature in determinig the marital status

#Predicitng missing values
pred_dep <-  predict(depFit,newdata=gtrain,type="class")

#Accuracy
p<-predict(depFit,gtrain,type="class")
acc=sum(p==gtrain[,3])/length(p)
acc

#Using Mode and checking accuracy
j_Mode <- Mode(dt,3)
j_Mode
pre=gtrain$job 
pre <-  j_Mode
acc=sum(pre==gtrain[,3])/length(p)
acc
#Accuracy of around 61% which is better than the mode value which gives 60% accuracy on train data


#impute missing job
dt_transform$marital[dt_transform$marital=="unknown"]<-predict(depFit,gtest,type="class")
summary(dt_transform$marital)
#Dropping unknown factor
dt_transform$marital <- factor(dt_transform$marital)
summary(dt_transform$marital)



#   Education
gtrain<-dt[dt$education!="unknown",]
gtest<-dt[dt$education=="unknown",]
depFit <- rpart(data=gtrain,education~.,xval=3)
fancyRpartPlot(depFit)

ggplot(dt,aes(x=age))+geom_histogram(aes(fill="education"),col="Black")+facet_grid(education~.)

ggplot(dt,aes(x=education))+geom_bar(aes(fill="education"),col="Black")



#Age and Job seems to be important feature in determinig the education details

#Predicitng missing values
pred_dep <-  predict(depFit,newdata=gtrain,type="class")

#Accuracy
p<-predict(depFit,gtrain,type="class")
acc=sum(p==gtrain[,4])/length(p)
acc
#52%
#Using Mode and checking accuracy
j_Mode <- Mode(dt,4)
j_Mode
pre=gtrain$education 
pre <-  j_Mode
acc=sum(pre==gtrain[,4])/length(p)
acc
#31%


#Accuracy of around 51.72% which is better than the mode value which gives 30.8% accuracy on train data


#impute missing job
dt_transform$education <- as.character(dt_transform$education)
dt_transform$education[dt_transform$education=="unknown"]<-predict(depFit,gtest,type="class")
summary(dt_transform$education)
#Dropping unknown factor
dt_transform$education <- factor(dt_transform$education)
summary(dt_transform$education)


#Default

#Considering Unknown values as one class which is representative of the people who has no credit records


#housing
table(dt$housing,dt$loan)
table(dt$loan)
length(dt[dt$loan=="unknown","housing"])
#ALL 990 observations has values in housing and loan as unknown.

ggplot(dt,aes(x=cons.price.idx))+geom_histogram(aes(fill="housing"),col="Black")+facet_grid(housing~.)
#Consumer price index seems to be a good proxy to impute missing value for housing loan

gtrain<-dt[dt$housing!="unknown",]
gtest<-dt[dt$housing=="unknown",]
depFit <- rpart(data=gtrain,housing~.,xval=3)
fancyRpartPlot(depFit)


#Predicitng missing values
pred_dep <-  predict(depFit,newdata=gtrain,type="class")

#Accuracy
p<-predict(depFit,gtrain,type="class")
acc=sum(p==gtrain[,6])/length(p)
acc
#55.4%
#Using Mode and checking accuracy
j_Mode <- Mode(dt,5)
j_Mode
pre=gtrain$housing 
pre <-  j_Mode
acc=sum(pre==gtrain[,5])/length(p)
acc
#79.1%


#Accuracy of around 71.9% which is better than the decision tree prediction which gives 55.1% accuracy on train data
#Will impute value using mode

#impute missing job
dt_transform$housing <- as.character(dt_transform$housing)
dt_transform$housing[dt_transform$housing=="unknown"]<-"no"
#Dropping unknown factor
dt_transform$housing <- factor(dt_transform$housing)
summary(dt_transform$housing)


#loan

#55.4%
#Using Mode and checking accuracy
j_Mode <- Mode(dt,6)
j_Mode
pre=gtrain$loan 
pre <-  j_Mode
acc=sum(pre==gtrain[,6])/length(p)
acc
#53.6%

#Will impute value using mode

#impute missing job
dt_transform$housing <- as.character(dt_transform$housing)
dt_transform$loan[dt_transform$loan=="unknown"]<-"yes"
#Dropping unknown factor
dt_transform$loan <- factor(dt_transform$loan)
summary(dt_transform$loan)

#tRANSFORMATIONS

hist(dt$age)
quantile(dt$age,seq(0,1,.1))
hist(log(dt$age))
quantile(log(dt$age),seq(0,1,.1))

dt_transform$age <- log(dt$age)


#Duration

hist(dt_transform$duration)
quantile(dt$duration,seq(0,1,.1))
hist(log1p(dt_transform$duration))
quantile(log1p(dt$duration),seq(0,1,.1))
boxplot(log1p(dt$duration))

dt_transform$duration <- log1p(dt_transform$duration)

#Campaign
unique(dt_transform$campaign)
summary(factor(dt_transform$campaign))
hist(dt_transform$campaign)
quantile(dt$campaign,seq(0,1,.1))
hist(log(dt_transform$campaign))
quantile(log(dt$campaign),seq(0,1,.1))
boxplot(log(dt$campaign))

dt_transform$campaign <- log(dt_transform$campaign)


#PDAYS-Drop

#previous
p <- as.factor(dt_transform$previous)
summary(p)
unique(dt_transform$previous)
hist(dt_transform$previous)
quantile(dt$previous,seq(0,1,.1))
hist(log(dt_transform$previous))
quantile(log(dt$campaign),seq(0,1,.1))
boxplot(log(dt$campaign))


#emp.var.rate
hist(dt_transform$emp.var.rate)
quantile(dt$emp.var.rate,seq(0,1,.1))
hist(1/(dt_transform$emp.var.rate))



#cons.price.index

hist(dt_transform$cons.price.idx)
quantile(dt_transform$cons.price.idx,seq(0,1,.1))
boxplot(dt_transform$cons.price.idx)
boxplot(1/sqrt(dt_transform$cons.price.idx))
quantile(1/sqrt(dt_transform$cons.price.idx),seq(0,1,.1))

dt_transform$cons.price.idx <- 1/sqrt(dt_transform$cons.price.idx)

#cons.conf.index
hist(dt_transform$cons.conf.idx)
quantile(dt_transform$cons.conf.idx,seq(0,1,.1))
boxplot(dt_transform$cons.conf.idx)
boxplot((dt_transform$cons.conf.idx))
quantile(1/(dt_transform$cons.price.idx),seq(0,1,.1))

#euribor3m
hist(dt_transform$euribor3m)
quantile(dt_transform$euribor3m,seq(0,1,.1))
boxplot(dt_transform$euribor3m)
boxplot((dt_transform$euribor3m))
quantile(1/(dt_transform$cons.price.idx),seq(0,1,.1))


# Split data into training (70%) and validation (30%)
dt1 = sort(sample(nrow(dt_transform), nrow(dt_transform)*.7))
train<-dt_transform[dt1,]
val<-dt_transform[-dt1,] 

# Check number of rows in training and validation data sets
nrow(train)
length(train$y[train$y=="yes"])
length(train$y[train$y=="no"])

nrow(val)
nrow(val[val$y=="yes",])

table(train$y)
25559/28831
#Benchmark Accuracy=88.6%
#Even if we predicted all observations as the ones who will not subscribe a bank term deposit
#we will be getting 88.6% of accuracy
#Using a model makes sense only when it gives accuracy greater than the benchmark accuracy


#Run Logistic Regression
mylogistic <- glm(y~ .-duration-pdays, data = train, family = "binomial")
summary(mylogistic)

#summary(mylogistic)$coefficient

#Stepwise Logistic Regression
# mylogit = step(mylogistic)
# summary(mylogit)

#Run Logistic Regression
mylogistic <- glm(y~ .-marital-education-housing-day_of_week-loan-pdays-previous-cons.conf.idx-nr.employed, data = train, family = "binomial")
summary(mylogistic)

#In addition, we can also perform an ANOVA Chi-square test to check the overall effect of variables 
#on the dependent variable.
anova(mylogistic,test = "Chisq")

#All the variable are significant except age .

#Removing age from the model
mylogistic <- glm(y~ .-age-marital-education-housing-day_of_week-loan-pdays-previous-cons.conf.idx-nr.employed, data = train, family = "binomial")
summary(mylogistic)


#Lets look at the actual vs predicted dataset
#Looking at 1st five observation in train dataset
head(train[,21],5)
#All observations has response value "no"
#Now lets see what is the output of these observations from the model
mylogistic$fitted.values[1:5]

mylogistic_decile <- cut(mylogistic$fitted.values,quantile(mylogistic$fitted.values,seq(0,1,.1)))
table(mylogistic_decile,train$y)
#We can see majority of observations having probability less than .3 as our dataset is imbalanced as
#it has more number of no as compared to yes


#Logistic Regression Coefficient
summary.coeff0 = summary(mylogistic)$coefficient

#Calculating Odd Ratios
OddRatio = exp(coef(mylogistic))
summary.coeff = cbind(Variable = row.names(summary.coeff0), OddRatio, summary.coeff0)
row.names(summary.coeff) = NULL

summary.coeff
#R Function : Standardized Coefficients
stdz.coff <- function (regmodel) 
{ b <- summary(regmodel)$coef[-1,1]
sx <- sapply(regmodel$model[-1], sd)
beta <-(3^(1/2))/pi * sx * b
return(beta)
}

std.Coeff = data.frame(Standardized.Coeff = stdz.coff(mylogit))
std.Coeff = cbind(Variable = row.names(std.Coeff), std.Coeff)
row.names(std.Coeff) = NULL

#Final Summary Report
final = merge(summary.coeff, std.Coeff, by = "Variable", all.x = TRUE)
final
#Prediction
pred = predict(mylogistic,val, type = "response")
finaldata = cbind(val, pred)

#Storing Model Performance Scores
library(ROCR)
pred_val <-prediction(pred ,val$y)

# Maximum Accuracy and prob. cutoff against it
acc.perf <- performance(pred_val, "acc")
ind = which.max( slot(acc.perf, "y.values")[[1]])
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]

# Print Results
print(c(accuracy= acc, cutoff = cutoff))



#Function for Lift and Gain
lift <- function(depvar, predcol, groups=10) {
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

finaldata1 <- finaldata
finaldata1$y <- ifelse(finaldata$y=="yes",1,0)
gain_lift_table = lift( finaldata1$y, finaldata1$pred, groups = 10)
gain_lift_table
#he Cumulative Lift of 4.3 for top two deciles, means that when selecting 20% of the records
#based on the model, one can expect 4.3 times the total number of targets (events) found by 
#randomly selecting 20%-of-records without a model

#85% of events covered in top 20% of data based on model.
# Plotting Lift curve
plot(performance(pred_val, measure="lift", x.measure="rpp"), colorize=TRUE)

# Plot the ROC curve
roc<- performance(pred_val, "tpr", "fpr")
plot(roc,colorize=T,
     main="ROC Curve",
     ylab="Sensitivity",
     xlab="1-Specificity")
#Benchmark Line
abline(a=0,b=1)

#Area Under the Curve
auc <- performance(pred_val,"auc")
auc <- unlist(slot(auc,"y.values"))
auc <- round(auc,4)
auc
legend(.6,.3,auc,title="AUC")
#AUC=.9381

#Calculating KS statistics
ks1.tree <- max(attr(acc.perf, "y.values")[[1]] - (attr(acc.perf, "x.values")[[1]]))
ks1.tree
#Ideally it should range between .40 to .70 
