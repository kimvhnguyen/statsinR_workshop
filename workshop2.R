### Statistics in R Workshop #2
## K. Nguyen, March 15, 2022

install.packages('gplots')

#load in packages to current R session
library(gplots)
library(car)
library(haven)
library(psych)
library(ggplot2)
library(devtools)
library(dplyr)
library(ggpubr)

#no exponential notation
options(scipen=100)
#tell R to print numericals with 4 digits
options(digits=4)

#setwd("C:/path/to/your/directory") #for PC users
setwd('/Users/KimNguyen/Desktop/Rworkshop')

#see what's in your working directory
dir()

#read in data as a data frame
data <- as.data.frame(read.delim('data.dat',header = F, sep = ""))

#add a generated group column to the data
set.seed(012874) #setting a randomization seed so we all get the same "random" list
a = sample(2,300,TRUE) #create a vector with 300 sampled numbers 1 and 2
data$time <- a #assign a new column "group" the vector "a"

#must make variable "group" a factor variable (telling R that it's categorical)
data$time <- as.factor(data$time)
data$time<-factor(data$time, labels=c("AM", "PM")) #renames the 1s as "AM" and 2s as "PM"
levels(data$time) #view levels of factor variable "time"

#create another categorical/factor variable for major with 1s and 2s
set.seed(1092)
b = sample (3,300, TRUE)
data$major <- b
data$major <- as.factor(data$major)
data$major<-factor(data$major, labels=c("psych", "bio", "chem")) #renames the 1s as "psych" and 2s as "bio" majors
levels(data$major)

#let's rename V1 as student satisfaction with the class
names(data)[names(data) == "V1"] <- "satisfaction"

#create a continuous variable for exam1 grades
set.seed(1209)
c = sample (c(65:100),300, TRUE)
data$exam1 <- c

set.seed(1920)
c = sample (c(18:30),300, TRUE)
data$age <- c

write.csv(data, "data_update.csv")
### goodness of fit tests
## binomial test
#H0: the proportion of AM students is equal to .5, π = 1/2
#HA: the proportion of AM students is not equal to .5, π ≠ 1/2

#binom.test(x,n,pi) #x = number of AM students, n = total number of students, pi = proportion
table(data$time) #get number of observations in each time level
binom.test(140, 300, 1/2)

#if pi is in the 95% confidence interval and p-value > .05, retain the null

## chi-square test
#H0: the ratio of AM:PM is 1:1
#HA: the ratio of AM:PM is not 1:1

observed <- table(data$time) #create an observed object with the counts of AM and PM
chisq.test(observed,p=c(1/2,1/2))$expected #plug observed object into chi square test 
#the $expected gives us the expected values of each group

chisq.test(observed,p=c(1/2,1/2)) #remove $expected to get results of chi square test

#p-value > .05, retain the null


### t-tests
## normality
time_exam1 <- gghistogram(data, x = "exam1",
                          add = "mean", rug = TRUE,
                          color = "time", fill = "time",
                          palette = "set2")

qqnorm(data$exam1[data$time == "AM"])
qqline(data$exam1[data$time == "AM"]) #data points are the circles, need to fall roughly on the line to be normal
shapiro.test(data$exam1[data$time == "AM"])

qqnorm(data$exam1[data$time == "PM"])
qqline(data$exam1[data$time == "PM"])
shapiro.test(data$exam1[data$time == "PM"])

# is this data normal?
#no! qq plot and shapiro test show non-normally distributed exam 1 scores for AM and PM

time_satis <- gghistogram(data, x = "satisfaction",
                        add = "mean", rug = TRUE,
                        color = "time", fill = "time",
                        palette = "set2")

qqnorm(data$satisfaction[data$time == "AM"])
qqline(data$satisfaction[data$time == "AM"])
shapiro.test(data$satisfaction[data$time == "AM"])

qqnorm(data$satisfaction[data$time == "PM"])
qqline(data$satisfaction[data$time == "PM"])
shapiro.test(data$satisfaction[data$time == "PM"])

#is this data normal?
#yes! qq plot checks out and shapiro test are p > .05

# one-sample t-tests
#we'll bypass normality assumption for now, but for real data you should do a transformation or nonparametric test (covered below)
#H0: the population mean (µ) of exam 1 grades in the AM class = 75%
#HA: the population mean (µ) of exam 1 grades in the AM class ≠ 75%

t.test((data$exam1[data$time == "AM"]), mu = 75) #mu is µ

#do we retain or reject the null? reject! p < .05
#what is the 95% confidence interval telling us?
#how do we find out if the average grade was less than or greater than 75%? next example line 136

# sign test for nonparametric distributions
#H0: The median exam 1 score for the AM class = 75%
#HA: The median exam 1 score for the AM class ≠ 75%

#sum(data$exam1[data$time == "AM"] > 75) #32
#binom.test(32,140,.5) #there are 32 exam1 grades in the AM class above 75%, there are 140 total sudents in the AM class

# one-sample t-test with a specific direction
#H0: the population mean (µ) of satisfaction in the PM class = 0
#HA: the population mean (µ) of satisfaction in the PM class > 0

t.test((data$satisfaction[data$time == "PM"]), mu = 0, alternative = 'greater')

#do we retain or reject the null? retain, p > .05
#why is out upper bound infinity? we set our alternative to be greater than a specified value without a specifed upper bound


# two-sample t-tests
# differences between two independent samples

#assumptions
box1 <- ggboxplot(data, x = "time", y = "satisfaction",
                  color = "time", palette = "set2", xlab = "time", ylab= "class time")

#create vectors for satisfaction for each time
AM_satis <- data$satisfaction[data$time == "AM"]
PM_satis <-data$satisfaction[data$time == "PM"]

#check for normality of distribution
qqnorm(AM_satis)
qqline(AM_satis)

qqnorm(PM_satis)
qqline(PM_satis)

#check for equal variances
leveneTest(satisfaction~time,data=data) #since we are telling the function the data set name, we don't have to use data$satisfaction and data$time
#did we pass? yes, p > .05 so we retain the null
#what does equal variances mean? variance of the group data are not too different, if they're sign. different we may be looking at bias in the data
#if this test failed, you would run a Welch's test for un-pooled standard error

#H0: the mean satisfaction is equal for AM and PM classes, µAM = µPM
#HA: the mean satisfaction is not equal for AM and PM classes, µAM ≠ µPM

t.test(AM_satis,PM_satis,var.equal=T) #to run Welch's test, use var.equal = F, because this test is only run if the varainces from levene's test are sig. different

# Mann-Whitney-Wilcoxon test for nonparametric data
#violation of normality assumption
box2 <- ggboxplot(data, x = "time", y = "exam1",
                  color = "time", palette = "set2", xlab = "time", ylab= "exam score")

#create vectors for exam score for each major
AM_exam <- data$exam1[data$time == "AM"]
PM_exam <-data$exam1[data$time == "PM"]

qqnorm(AM_exam)
qqline(AM_exam)

qqnorm(PM_exam)
qqline(PM_exam)

leveneTest(exam1~time,data=data)

#Mann-Whitney_Wilcoxon test for nonparametric two-sample comparisons
#H0: The distribution exam 1 scores for the AM and PM classes are the same.
#HA: The distribution exam 1 scores for the AM and PM classes are not the same.
wilcox.test(exam1 ~ time, data=data) 


### ANOVA
## one-way, with categorical variable with > 2 levels
#check assumptions
box3 <- ggboxplot(data, x = "major", y = "exam1",
                  color = "major", palette = "set2", xlab = "major", ylab= "exam score")

leveneTest(exam1~major,data=data)

psych_exam <- data$exam1[data$major == "psych"]
bio_exam <-data$exam1[data$major == "bio"]
chem_exam <-data$exam1[data$major == "chem"]

qqnorm(psych_exam)
qqline(psych_exam)

qqnorm(bio_exam)
qqline(bio_exam)

qqnorm(chem_exam)
qqline(chem_exam)

#H0: the mean score on exam 1 were equal for all three majors, µpsych = µbio = µchem
#HA: the mean score on exam 1 were not equal for all three majors, µpsych ≠ µbio ≠ µchem

model1 <- aov(data$exam1 ~ data$major)

summary(model1)

#post-hoc tests
#if the anova was significant for the major group, where do the differences exist?
#tukey test will run all possible pairwise-comparisons w/ t test
TukeyHSD(model1)

#nonparametric test, Kruskal-Wallis test
kruskal.test(data$exam1 ~ data$major)

## multi-factor anova
#now we want to have major and time in the same model to predict exam 1 score
#check assumptions
box4 <- ggboxplot(data, x = "major", y = "exam1",
                  color = "time", palette = "set2", xlab = "major", ylab= "exam score")

violin1 <- ggplot(data, aes(x=time, y=exam1))+ geom_violin(aes(color = major), trim = FALSE, position = position_dodge(0.9)) +
  geom_boxplot(aes(color = major), width = 0.05, position = position_dodge(0.9)) +
  labs(title="", x = "Major",y = "Exam 1 Score")+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#9999CC")) + theme_classic()

#main effects model
#H0: while accounting for major, there is no difference in mean exam 1 score between the two classes. 
#HA: while accounting for major, there is a difference in mean exam 1 score between the two classes. 

#H0: while accounting for class time, there is no difference in mean exam 1 score between the three majors. 
#HA: while accounting for class time, there is a difference in mean exam 1 score between the three majors. 


model2 <- aov(data$exam1 ~ data$major + data$time)
drop1(model2,~.,test='F')
#gives us main effects of our categorical variables
#when you report the results, you are controlling for the other variable

#interaction model

#H0: there is no interaction between class time and major on the mean exam 1 score
#HA: there is an interaction between class time and major on the mean exam 1 score

model3 <- aov(data$exam1 ~ data$major*data$time)
drop1(model3,~.,test='F')

# Interaction plot for two categorical explanatory variables
interaction.plot(x.factor = data$major, trace.factor = data$time, 
                 response = data$exam1, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Major", ylab="Class Time",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800", "#9999CC"))


