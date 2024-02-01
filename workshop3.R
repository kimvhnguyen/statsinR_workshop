### Statistics in R Workshop #3
## K. Nguyen, March 22, 2022

install.packages('Hmisc')
install.packages('apaTables')

#load in packages to current R session
library(gplots)
library(car)
library(haven)
library(psych)
library(ggplot2)
library(devtools)
library(dplyr)
library(ggpubr)
library(Hmisc)
library(apaTables)


#no exponential notation
options(scipen=100)
#tell R to print numericals with 4 digits
options(digits=4)

#setwd("C:/path/to/your/directory") #for PC users
#always double check that you have the correct path to your folder!!!
setwd('/Users/KimNguyen/Desktop/Rworkshop')

#reading in a .csv file
data <- as.data.frame(read.csv('data_update.csv'))

#subsetting the dataset, just want to get rid of column variables that we are not using in this script
data <- data[-c(3:17)]

names(data)
describe(data)


### correlation
#H0: x variable and y variable are not correlated (p = 0). 
#HA: x variable and y variable are correlated (p ≠ 0). 

## assumption of correlation
#checking the relationship between our two variables is linear

#run lines 47-51 together to create the full plot!
age_exam1 <- ggplot(data, aes(y=exam1, x=age)) +
  labs(x = "age (years)", y = "exam score") +
  geom_point() +
  geom_smooth(method=lm , color="Red", fill="Red", se=TRUE) + theme_classic()
age_exam1 + theme(title = element_text(size = 16), axis.text = element_text(size = 14),axis.title = element_text(size = 15))

#same here, run 54-58 together
age_satis <- ggplot(data, aes(y=satisfaction, x=age)) +
  labs(x = "age (years)", y = "satisfaction") +
  geom_point() +
  geom_smooth(method=lm , color="Red", fill="Red", se=TRUE) + theme_classic()
age_satis + theme(title = element_text(size = 16), axis.text = element_text(size = 14),axis.title = element_text(size = 15))

#run 61-65 together
exam1_satis <- ggplot(data, aes(y=exam1, x=satisfaction)) +
  labs(x = "satisfaction", y = "exam score") +
  geom_point() +
  geom_smooth(method=lm , color="Red", fill="Red", se=TRUE) + theme_classic()
exam1_satis + theme(title = element_text(size = 16), axis.text = element_text(size = 14),axis.title = element_text(size = 15))

#are these linear relationships?
#generally yes, although the degree of association is different

#conduct correlation

cor.test(data$exam1, data$satisfaction)
cor.test(data$age, data$satisfaction)
cor.test(data$age, data$exam1)

#create a correlation table with all contiuous variables in the data frame
#output table as a .doc file
data_cont <- data[-c(1,3,4)] #subset data with only continuous vars, this function will not work if you have categorical vars in the dataset
apa.cor.table(data_cont, filename="corr_all.doc", table.number=1) #output .doc is in your working directory

### regression
#H0: x variable is not a linear predictor of y variable (β = 0). 
#HA: x variable is a linear predictor of y variable (β ≠ 0). 

## first we have to build our linear models
#lm is a function to create linear models
m1 <- lm(data$exam1 ~ data$satisfaction) #structure of function is DV ~ IV
m2 <- lm(data$exam1 ~ data$age)
m3 <- lm(data$satisfaction ~ data$age)

## normality assumption

qqnorm(m1$residuals)
qqline(m1$residuals)

# residual plot to look for funneling, checking for equal variance, otherwise called heteroschedasticity
plot(m1$fitted.values, m1$residuals, xlab='fitted values', ylab='residuals')
abline(h=0)

qqnorm(m2$residuals)
qqline(m2$residuals)
plot(m2$fitted.values, m2$residuals, xlab='fitted values', ylab='residuals')
abline(h=0)

qqnorm(m3$residuals)
qqline(m3$residuals)
plot(m3$fitted.values, m3$residuals, xlab='fitted values', ylab='residuals')
abline(h=0)

#are any of these model residuals normal?
#looks like it, no visible funnelling from the y=0, datapoints look evenly distributed above and below the y=0 line

#assumptions met, view the regression statistics
summary(m1)
summary(m2)
summary(m3)

### glm

#H0: when controlling for class time, satisfaction is not a linear predictor of exam 1 score. 
#H0: when controlling for class time, satisfaction is a linear predictor of exam 1 score.
#H0: when controlling for satisfaction, class time is not a linear predictor of exam 1 score. 
#H0: when controlling for satisfaction, class time is a linear predictor of exam 1 score.

## build the model first
m4 <- lm(exam1 ~ satisfaction + time, data=data) #here, we are adding in our categorical var. "time" the general function is DV ~ IV1 + IV2

## assumptions
#again, run 130 - 134 together
m4_plot <- ggplot(data, aes(x=satisfaction, y=exam1, color= time)) +
  labs(x = "satisfaction", y= "exam score")+
  geom_point() +
  geom_smooth(method=lm , se=TRUE, aes(fill=time), fullrange = TRUE) + theme_classic()
m4_plot + theme(title = element_text(size = 13), axis.text = element_text(size = 14),axis.title = element_text(size = 15)) + scale_color_manual(values = c("steelblue4", "sienna3")) + scale_fill_manual(values = c("steelblue4", "sienna3"))

summary(m4)

#interaction

#H0: the effect of satisfaction on exam 1 score is the same for AM and PM classes. 
#HA: the effect of satisfaction on exam 1 score is not the same for AM and PM classes. (there is an interaction between satisfaction x time on exam 1)

m4_int <- lm(exam1 ~ satisfaction*time, data=data) #to run an interaction you multiply the effects of both variables
summary(m4_int)



