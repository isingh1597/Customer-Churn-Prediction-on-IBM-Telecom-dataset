library(glmnet)
library(boot)
library(ISLR)
library(plyr)
library(ROCR)
library(survival)

#Read file + remove missing values 
tel.obj=read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
tel.obj = na.omit(tel.obj)

#Changing "no internet service" to "no"
new <- c(10:15)
for(i in 1:ncol(tel.obj[,new])) 
{
  tel.obj[,new][,i] <- as.factor(mapvalues(tel.obj[,new][,i], from =c("No internet service"),to=c("No")))
}

#Changing "no phone service" to "no" 
tel.obj$MultipleLines <- as.factor(mapvalues(tel.obj$MultipleLines, from=c("No phone service"),to=c("No")))

#Removing insignificant predictors from x 
tel.obj$customerID <- NULL

#Changing levels of Churn from "No" and "Yes" to "1" and "0" respectively 
tel.obj$Churn <- as.numeric(tel.obj$Churn)
tel.obj$Churn <- ifelse(tel.obj$Churn==1,0,1)

#Creating x matrix with all predictors except customerIDs
x = model.matrix(tenure~.-Churn,tel.obj)[,-1]
#Creating y matrix using Surv function 
y = Surv(time = tel.obj$tenure, event = tel.obj$Churn)

#Fit model 
mod.cox = glmnet(x, y, family = "cox", alpha = 0.9)
#Cross validation for optimal tuning parameter 
cv.cox = cv.glmnet(x ,y, family = "cox")
plot(cv.cox)
best.lam = cv.cox$lambda.1se

#Coefficient estimates 
f = coef(mod.cox, s = best.lam)
#Calculating Hazard Ratios 
HR = list()
HR = exp(f@x)

#Smooth estimates beta 
co = Coxph(y~.-Churn-tenure, data = tel.obj)

#Kaplan Meier Curve 
plot(survfit(y~tel.obj$Contract, data = tel.obj), col = 1:3)

hr = hare(data = tel.obj$tenure, delta = tel.obj$Churn, cov = x)

#Function with probability information to calculate MRL 
fit = survfit(y~tel.obj$InternetService, data = tel.obj)
summary(fit)


i=1                                    
n_levels = count(fit$strata)$freq
size = count(fit$strata)$x    #Equal for all levels= 72
lim = n_levels*size
counter = size 

while(counter<=lim){
prob = summary(fit, times = )$surv #[i:counter]            #Survival probabilities
xaxi = list()
yaxi = list()
i=1
#Loop for 1 level out of 3 or 4 
while(i<=size) {
  thresh = i          #Threshold has to go from 1-72 for all levels ##Depends on i so i needs increment and resetting
  den = prob[thresh]  #Denominator storing S(t) at every threshold t  
  value = thresh
  num = 0             #Numerator of MRL 
#At every threshold sum the probabilities after 
var = seq(value,size)
num = sum(prob[var])
mrl = num/den
  yaxi = append(yaxi, mrl)
  xaxi = append(xaxi, thresh)
  i= i+1 
}
if(counter==size){
  par(mar = c(5,5,4,2),cex=0.9)
  plot(xaxi, yaxi, xlab ="time (months)", ylab = "mrl", type = "p", col="red", ylim = c(0,80))
} else if(counter==(size*2)){
    lines(xaxi, yaxi, col="blue")
} else if(counter==(size*3)) {
    lines(xaxi, yaxi, col="green")
} else {
  lines(xaxi, yaxi, col="black")
}
i= (counter+1)
counter = counter + size
}
legend("topright", c("DSL","FiberOptic","No Internet"), fill = c("red","blue","green"), cex = 0.9)