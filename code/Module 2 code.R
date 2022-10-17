library(tidyverse)
library(Metrics)
library(corrplot)
bodyFat=read.csv("~/Downloads/BodyFat.csv",header=TRUE)

#summary of data
summary(bodyFat)

#histogram for body fat %
ggplot()+
  geom_histogram(aes(x=bodyFat$BODYFAT),fill="white",color="black",binwidth=1)+
  theme(
    plot.title= element_text(hjust=0.5))+
  labs(
    title="Histogram for Body Fat",
    x="Body Fat %",
    y="Count"
    
  )+
  scale_y_continuous(expand=c(0,0))
#histogram for weight
ggplot()+
  geom_histogram(aes(x=bodyFat$WEIGHT),fill="white",color="black",binwidth=5)+
  theme(
    plot.title= element_text(hjust=0.5))+
  labs(
    title="Histogram for Weight",
    x="Weight(lbs)",
    y="Count"
    
  )+
  scale_y_continuous(expand=c(0,0))
#histogram for height
ggplot()+
  geom_histogram(aes(x=bodyFat$HEIGHT),fill="white",color="black",binwidth=1)+
  theme(
    plot.title= element_text(hjust=0.5))+
  labs(
    title="Histogram for Height",
    x="Height(in)",
    y="Count"
    
  )+
  scale_y_continuous(expand=c(0,0))
#histogram for abdomen
ggplot()+
  geom_histogram(aes(x=bodyFat$ABDOMEN),fill="white",color="black",binwidth=1)+
  theme(
    plot.title= element_text(hjust=0.5))+
  labs(
    title="Histogram for Abdomen Circumference",
    x="Abodomen Circumference(cm)",
    y="Count"
    
  )+
  scale_y_continuous(expand=c(0,0))


#bodyfat can't be equal to 0 but when we recalculate using Siri's Equation we get a negative number. 
bodyFat2<-bodyFat%>%
  mutate(new_bodyFat=(495/DENSITY)-450) #creates a column for bodyfat using Siri's equation just for comparison
#remove observation 182
bodyFat=bodyFat[-182,]
#adjust the row number after deleting observation 182
row.names(bodyFat) <- 1:nrow(bodyFat)
#recalculate height for observation 42 using ADIPOSITY and WEIGHT(BMI FORMULA)
new_height=round(sqrt((bodyFat[42,5]*703)/bodyFat[42,7]),2)
bodyFat[42,6]=new_height
#create a new csv file with clean dataset
write.csv(bodyFat,"~/Desktop/newBody_Fat.csv")

#correlation matrix
corrplot(cor(bodyFat),method="number")

#Modeling
model=lm(BODYFAT~ABDOMEN,data=bodyFat)
summary(model)
#Calculate RMSE
rmse(bodyFat$BODYFAT,predict(model))
#check for linearity and homoscedasticity
plot(predict(model),resid(model),xlab="Predicted Body Fat",ylab="Standardized Residuals")+abline(h=0)
#check for normality
qqnorm(rstandard(model))
qqline(rstandard(model))

#Trying different combinations of predictors
model1=lm(BODYFAT~ABDOMEN+HEIGHT,data=bodyFat)
summary(model1)

model2=lm(BODYFAT~ABDOMEN+AGE,data=bodyFat)
summary(model2)

model3=lm(BODYFAT~ABDOMEN+THIGH,data=bodyFat)
summary(model3)

model4=lm(BODYFAT~ABDOMEN+HIP,data=bodyFat)
summary(model4)

model5=lm(BODYFAT~ABDOMEN + WEIGHT,data=bodyFat)
summary(model5)

model6=lm(BODYFAT~ABDOMEN+WRIST,data=bodyFat)
summary(model6)

model7=lm(BODYFAT~ABDOMEN+FOREARM,data=bodyFat)
summary(model7)

#model5 has the highest R-squared and significant predictors so let's check model diagnostics
model5=lm(BODYFAT~ABDOMEN + WEIGHT,data=bodyFat)
summary(model5)
#Calculate RMSE
rmse(bodyFat$BODYFAT,predict(model5))
#check for linearity and homoscedasticity
plot(predict(model5),resid(model5),xlab="Predicted Body Fat",ylab="Standardized Residuals")+abline(h=0)
#check for normality
qqnorm(rstandard(model5))
qqline(rstandard(model5))

#Trying adding different combinations of predictors using model5
model6=lm(BODYFAT~ABDOMEN + WEIGHT +BICEPS,data=bodyFat)
summary(model6)

model7=lm(BODYFAT~ABDOMEN + WEIGHT +AGE,data=bodyFat)
summary(model7)

model8=lm(BODYFAT~ABDOMEN + WEIGHT + THIGH,data=bodyFat)
summary(model8)

model9=lm(BODYFAT~ABDOMEN + WEIGHT + HIP,data=bodyFat)
summary(model9)

model10=lm(BODYFAT~ABDOMEN + WEIGHT + FOREARM,data=bodyFat)
summary(model10)

model11=lm(BODYFAT~ABDOMEN + WEIGHT + WRIST,data=bodyFat)
summary(model11)

#model11 has the highest R-squared and significant predictors so let's check model diagnostics
model11=lm(BODYFAT~ABDOMEN + WEIGHT + WRIST,data=bodyFat)
summary(model11)
#Calculate RMSE
rmse(bodyFat$BODYFAT,predict(model11))
#check for linearity and  homoscedasticity
plot(predict(model11),resid(model11),xlab="Predicted Body Fat",ylab="Standardized Residuals",main="Residual Plot")+abline(h=0)
#check for normality
qqnorm(rstandard(model11))
qqline(rstandard(model11))
shapiro.test(model11$res)

## Additional linearity assumption
par(mfrow = c(1,3))
plot(bodyFat$ABDOMEN, bodyFat$BODYFAT, xlab="Abdomen circumference (cm)", ylab="Body Fat Percentage", main="Abdomen vs % Body Fat")
plot(bodyFat$WEIGHT, bodyFat$BODYFAT, xlab="Weight (lbs)", ylab="Body Fat Percentage", main="Weight vs % Body Fat")
plot(bodyFat$WRIST, bodyFat$BODYFAT, xlab="Wrist circumference (cm)", ylab="Body Fat Percentage", main="Wrist vs % Body Fat")

## Additional independence assumption
par(mfrow = c(1,3))
plot(bodyFat$ABDOMEN,rstandard(model11),xlab="Abdomen",ylab="Standardized Residuals", main="Abdomen vs Residuals plot")+abline(h=0)
plot(bodyFat$WEIGHT,rstandard(model11),xlab="Weight",ylab="Standardized Residuals", main="Weight vs Residuals plot")+abline(h=0)
plot(bodyFat$WRIST,rstandard(model11),xlab="Wrist",ylab="Standardized Residuals", main="Wrist vs Residuals plot")+abline(h=0)

#predict observation for a man
predict(model11,data.frame(ABDOMEN=95.6,WEIGHT=177,WRIST=17.7),interval="predict")
