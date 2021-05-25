#Install glmnet package
install.packages("glmnet", repos = "http://cran.us.r-project.org")
install.packages("ROCR")

#Library for helper functions
library(glmnet)
library(boot)
library(ISLR)
library(plyr)
library(ROCR)

#Read file
tel.obj=read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

#Dataset exploration
str(tel.obj)

#Checking number of missing values and removing them
sum(is.na(tel.obj))                             # 11
tel.obj = na.omit(tel.obj)

#Changing "no internet service" to "no"
new <- c(10:15)
for(i in 1:ncol(tel.obj[,new])) 
{
tel.obj[,new][,i] <- as.factor(mapvalues(tel.obj[,new][,i], from =c("No internet service"),to=c("No")))
}

#Changing "no phone service" to "no" 
tel.obj$MultipleLines <- as.factor(mapvalues(tel.obj$MultipleLines, from=c("No phone service"),to=c("No")))

#Checking correlations between numericals vectors
cor(tel.obj$tenure,tel.obj$TotalCharges)        # 0.8258805
cor(tel.obj$tenure,tel.obj$MonthlyCharges)      # 0.2468618
cor(tel.obj$MonthlyCharges,tel.obj$TotalCharges)# 0.6510648
  
#Checking range of Monthly Charges and TotalCharges
range(tel.obj$MonthlyCharges)                   # 18.25 118.75
range(tel.obj$TotalCharges, na.rm = TRUE)       # 18.8 8684.8 (Contained all the missing values)


#Creating x matrix with all predictors except customerID
x = model.matrix(Churn~.-customerID-TotalCharges,tel.obj)[,-1]
#Creating y vector from response Churn 
y = tel.obj$Churn

#Train Test split in 80/20 ratio
set.seed(101)
train=sample(1:nrow(tel.obj),nrow(tel.obj)*0.8)
x = data.frame(x)
x.train = x[train,]
#Change dataframe to matrix to make it compatible w/ glmnet
x.train = as.matrix(x.train)
#Same for test set
x.test = x[-train,]
x.test = as.matrix(x.test)

#Create vector of lambda values from 10^-1 to 10^10
grid = 10^seq(10,-2,length = 100)

#Fit model on training set (LASSO)
log.mod = glmnet(x.train, y[train], alpha = 0.95, lambda = grid, family = "binomial")
plot(log.mod)                  #Tuning parameters vs Coefficient estimates

#Cross validation to find optimal tuning parameter lambda
set.seed(1)
cvfit = cv.glmnet(x.train,y[train], family = "binomial",type.measure = "class")
plot(cvfit)
#Value of lambda that gives least cross validated error 
bestlambda = cvfit$lambda.min
bestlam = cvfit$lambda.1se

#Predicting Churn on test set using model fit earlier 
log.pred = predict(log.mod, newx = x.test, type = "class", s=bestlam)

#Confusion Matrix
y.test = y[-train]
table(log.pred, y.test)
mean(log.pred == y.test)
#Prediction accuracy = 0.81449

##Coefficient estimates 
#Fit model on full data set 
log.full = glmnet(as.matrix(x), y, alpha = 0.95, lambda = bestlam, family = "binomial")
lasso.pred = predict(log.full, type = "coefficients", s=bestlam)[1:23,]
#List of coefficient estimates of predictors  
lasso.pred[lasso.pred!=0]

#Using glm on predictors selected by lasso 
glm.fit = glm(Churn~SeniorCitizen+Dependents+tenure+MultipleLines+InternetService+OnlineSecurity+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod, data = tel.obj, family = binomial)
summary(glm.fit)
glm.prob = predict(glm.fit, type = "response")

pr <- prediction(glm.prob, tel.obj$Churn)
prf <- performance(pr, measure = "tpr",x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc1 <- auc@y.values 
auc1    #0.84481 


table(glm.pred, tel.obj$Churn)
  #Average misclassification rate= 0.8030432

# k fold cross validation with k=10 
library(caret)
cntrl <- trainControl(method = "cv", number = 10)
model <- train(Churn~SeniorCitizen+Dependents+tenure+MultipleLines+InternetService+OnlineSecurity+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod, data = tel.obj, trControl = cntrl, method = "glm", family = binomial())
model$results                # Accuracy = 0.8058858
summary(model)
probs = predict(model, type = "prob")
probs$No<-NULL
preds = rep("No",7032)
preds[probs>0.5]="Yes"
preds = as.factor(preds)
mean(preds==tel.obj$Churn)      #0.8030432
table(preds, tel.obj$Churn)

######Cost Model 
fit = glm(Churn~SeniorCitizen+Dependents+tenure+MultipleLines+InternetService+OnlineSecurity+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod, data = tel.obj, family=binomial)
churn.probs <- predict(fit, type="response")

# threshold vector
thresh <- seq(0.05,1, length = 20)
#cost vector
cost = rep(0,length(thresh))
# cost as a function of threshold
for (i in 1:length(thresh)){
  glm.pred = rep("No", length(glm.prob))
  glm.pred[glm.prob > thresh[i]] = "Yes"
  glm.pred= as.factor(glm.pred)
  x1 <- confusionMatrix(glm.pred, tel.obj$Churn, positive = "Yes")
  TN <- x1$table[1]/7032
  FP <- x1$table[2]/7032
  FN <- x1$table[3]/7032
  TP <- x1$table[4]/7032
  cost[i] = FN*315 + TP*63 + FP*63 + TN*0 
}

# standard model - threshold 0.5
glm.pred = rep("No", length(churn.probs))
glm.pred[churn.probs > 0.5] = "Yes"
glm.pred <- as.factor(glm.pred)

x1 <- confusionMatrix(glm.pred, tel.obj$Churn, positive = "Yes")
TN <- x1$table[1]/7032
FP <- x1$table[2]/7032
FN <- x1$table[3]/7032
TP <- x1$table[4]/7032
cost_simple = FN*315 + TP*63 + FP*63 + TN*0

# putting results in a dataframe for plotting
df <- data.frame(model = c(rep("optimized",20),"simple"), cost_per_customer = c(cost,cost_simple), threshold = c(thresh,0.5))

# plotting
ggplot(df, aes(x = threshold, y = cost_per_customer, group = model, colour = model)) + geom_line() + geom_point()

# cost savings of optimized model (threshold = 0.2) compared to baseline model (threshold = 0.5)
savings_per_customer = cost_simple - min(cost)

total_savings = 1000000*savings_per_customer  #10.374

total_savings #10,374,573 


tb <-table(tel.obj$Churn, tel.obj$MultipleLines)
sum_sen<-sum(tb[1,2],tb[2,2])
sum_notsen<-sum(tb[1,1],tb[2,1])
tb[1,1]<-(tb[1,1]/sum_notsen)*100
tb[1,2]<-(tb[1,2]/sum_sen)*100
tb[2,1]<-(tb[2,1]/sum_notsen)*100
tb[2,2]<-(tb[2,2]/sum_sen)*100

#Code for 3 factor levels ##########################
sum_no<-sum(tb[1,1],tb[2,1])
sum_nps<-sum(tb[1,2],tb[2,2])
sum_yes<-sum(tb[1,3],tb[2,3])
tb[1,1]=(tb[1,1]/sum_no)*100
tb[2,1]=(tb[2,1]/sum_no)*100
tb[1,2]=(tb[1,2]/sum_nps)*100
tb[2,2]=(tb[2,2]/sum_nps)*100
tb[1,3]=(tb[1,3]/sum_yes)*100
tb[2,3]=(tb[2,3]/sum_yes)*100
par(mar = c(6,5,6,2),cex=0.8)
barplot(tb,col = coul, width = 1, xlim = c(0,5), names.arg = c("Month-to-Month","One Year","Two Year"))
legend("right",inset= c(0,-1), c("Churn","Not Churn"),fill = coul)
title(ylab = "Percentage")
####################################

#Code for 4 factor levels ##########################
sum_bt<-sum(tb[1,1],tb[2,1])
sum_cc<-sum(tb[1,2],tb[2,2])
sum_ec<-sum(tb[1,3],tb[2,3])
sum_mc<-sum(tb[1,4],tb[2,4])
tb[1,1]=(tb[1,1]/sum_bt)*100
tb[2,1]=(tb[2,1]/sum_bt)*100
tb[1,2]=(tb[1,2]/sum_cc)*100
tb[2,2]=(tb[2,2]/sum_cc)*100
tb[1,3]=(tb[1,3]/sum_ec)*100
tb[2,3]=(tb[2,3]/sum_ec)*100
tb[1,4]=(tb[1,4]/sum_mc)*100
tb[2,4]=(tb[2,4]/sum_mc)*100
par(mar = c(6,5,6,2),cex=0.7)
barplot(tb,col = coul, width = 0.8, xlim = c(0,5), names.arg = c("Bank Transfer","Credit Card","E-Check","Mailed Check"))
legend("right",inset= c(0,-1), c("Churn","Not Churn"),fill = coul)
title(ylab = "Percentage")
####################################

library(RColorBrewer)
coul <- brewer.pal(3, "Pastel2") 
par(mar = c(6,5,6,2),cex=0.75)
barplot(tb,col = coul, width = 1.5, xlim = c(0,5), names.arg = c("No Multiple Lines","Yes Multiple Lines"))
legend("right",inset= c(0,-1), c("Churn","Not Churn"),fill = coul)
title(ylab = "Percentage")
