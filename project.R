require(corrplot)
require(ggplot2)
library(MASS)
require(leaps)
require(HH)
library(readr)
day <- read_csv("C:/Users/mom1/Downloads/Bike-Sharing-Dataset/day.csv")

ggplot(day, aes(as.factor(season), cnt)) + geom_boxplot()
ggplot(day, aes(as.factor(weathersit), registered)) + geom_boxplot()
ggplot(day, aes(as.factor(yr), cnt)) + geom_boxplot()
ggplot(day, aes(as.factor(workingday), cnt)) + geom_boxplot()
ggplot(day, aes(as.factor(holiday), cnt)) + geom_boxplot()

M <- cor(day)
corrplot(M, type="lower",method = "number")

#From the above corelation plot we see that the 'temp','hum' and 'windspeed' are good 
#predictors for fitting the model
lm.model<-lm(cnt ~ as.factor(holiday) + as.factor(workingday) + temp + hum + windspeed,
             data=day)
summary(lm.model)
plot(lm.model)

lm.model1<-lm(cnt ~ as.factor(holiday) + as.factor(workingday) + temp + hum + windspeed +
                as.factor(season),data=day)
summary(lm.model1)
plot(lm.model1)

plot(day$holiday,lm.model1$residuals)
plot(day$workingday,lm.model1$residuals)
plot(day$temp,lm.model1$residuals)
plot(day$hum,lm.model1$residuals)
plot(day$windspeed,lm.model1$residuals)

#residual vs temp plot we see there is some curvature
X=(day$temp)^2
lm.model2<-lm(cnt ~ as.factor(holiday) + as.factor(workingday) + temp + X +hum + 
                windspeed + as.factor(season),data=day)
summary(lm.model2)
plot(lm.model2)

#The standard error value for temp increased alot eventhough p-value is significant
#p-value is not significant for workingday remove it

lm.model3<-lm(cnt ~ as.factor(holiday) + temp + hum + windspeed + as.factor(season),data=day)
summary(lm.model3)
plot(lm.model3)

#applied the box-cox transformation for the response variable and fit the workingday
y=log(day$cnt)
lm.model4<-lm(y ~ as.factor(holiday) + as.factor(workingday) +temp + hum + 
                windspeed + as.factor(season),data=day)
summary(lm.model4)
plot(lm.model4)

#yr
lm.model5<-lm(cnt ~ as.factor(holiday) + temp + hum + windspeed + 
                as.factor(season)+ as.factor(yr),data=day)
summary(lm.model5)
plot(lm.model5)

data=cbind(mean(vif(lm.model)),mean(vif(lm.model1)),mean(vif(lm.model2)),mean(vif(lm.model3)),
           mean(vif(lm.model3)),mean(vif(lm.model5)))
data
#The maximum leverage value should be (2*(k+1)/n) which is approximately 0.025
lev=hat(model.matrix(lm.model5))
plot(lev)
day[lev>0.025,c(1)]
nrow(day[lev>0.025,c(1)])


point=c(17,50,52,69,105,150,185,248,283,315,328,360,367,381,416,433,472,514,551,612,647,682,692,725)
r=rstudent(lm.model5)
plot(r)
for(i in point){ 
  cook=cooks.distance(lm.model6) 
  plot(cook) 
  points(i,cook[i],col='red')
}


#observation 69 and 692 are influential remove that data and fitting the model again.
day=day[c(-69,-692),]
lm.model6<-lm(cnt ~ as.factor(holiday) + temp + hum + windspeed + 
                as.factor(season)+ as.factor(yr),data=day)
summary(lm.model6)
plot(lm.model6)
hist(lm.model6$residuals)
ggplot(day,aes(cnt))+geom_histogram(binwidth = 200)

#Poisson distribution can be used to analyze count data.
mean=mean(day$cnt)
variance=var(day$cnt)
ratio=variance/mean
cbind(mean,variance,ratio)
#For the count data, if the variance is larger than mean the data has over dispersion.

#standard poisson distribution
glm.model1<-glm(cnt ~ as.factor(holiday) + as.factor(workingday) + temp + hum + windspeed + 
                  as.factor(season),family='poisson',data=day)
summary(glm.model1)
#p values are significant but Residual deviance and degree of freedom suggest that model 
#doesnt fit the data well probably because of over dispersion

#To resolve over-dispersion, we use 2 models, namely 1.Quassi-Poisson Regression
#QP estimates scale parameter and fixes the estimated standard error
glm.model2<-glm(cnt ~ as.factor(holiday) + as.factor(workingday) + temp + hum + windspeed + 
                  as.factor(season),family=quasipoisson(link="log"),data=day)
summary(glm.model2)
#estimated standard errors in QP are larger than in P model.Dispersion parameter in QP 422.3375
#means there is indeed over dispersion in data set and P underestimated the standard errors.

#2.Negative Binomial model contains an extra parameter theta, which is the multiplicative random effect.
glm.model3<-glm.nb(cnt ~ as.factor(holiday) + as.factor(workingday) + temp + hum + windspeed +
                     as.factor(season),data=day)
summary(glm.model3)
#chi square test
pchisq(1531.96-746.12,df=(728-720),lower.tail = FALSE)
#p-value less than 0.05 So nb model is also fitting the model correctly
