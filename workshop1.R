### Statistics in R Workshop #1
## K. Nguyen, March 8, 2022
# Introduction to R and Data Management

#install necessary packages
#all R packages have its own documentation 
install.packages("car")
install.packages("haven")
install.packages("psych")
install.packages('ggplot2')
install.packages("devtools")
install.packages("dplyr")
install.packages("ggpubr")

#load in packages to current R session
library(car)
library(haven)
library(psych)
library(ggplot2)
library(devtools)
library(dplyr)
library(ggpubr)

#some R output settings
#no exponential notation
options(scipen=100)
#tell R to print numericals with 4 digits
options(digits=4)

### Arithmetic

#you can use R as a calculator
7+5
7*5
10/3
7^5

### Functions
log(81)
sqrt(81)

#errors, NaN =  not a number
log(-81)

### Creating and Manipulating Objects

#assign objects
object <- 20
object

#overwrite previously assigned object
object <- 30
object

#save function output as objects
result <- sqrt(9000)
result

### Vector Objects
## a vector is a list of values, the "c" is for concatenate
# you can perform arithmetic on whole vectors

vector <- c(1,2,3,4,5)
vector

vector2 <- 2*vector
vector2

#what does this line do?
vector3 <- c(vector, vector2)

#vector properties
length(vector)
mean(vector)
sd(vector)
sort(vector)

## Indexing
# calling specific values from a vector
vector3[2]

## Logical statements
vector3 > 5
vector3 == 5
vector3 != 5

#index by logical statement
#what if I want to index by a logical expression?
vector3[vector3 > 4]

#what if I want to index by multiple expressions at once?
vector3[vector3 > 4 | vector3 !=10 ] #this is giving me values within the vector that are greater than 4 OR equal to 10
vector3[vector3 > 4 & vector3 ==10 ] #this is giving me values greater than 4 AND equal to 10

### Working with Data

#set your working directory
#getwd()
#setwd('/path/to/your/directory') #for mac users
#setwd("C:/path/to/your/directory") #for PC users
setwd('/Users/KimNguyen/Desktop/Rworkshop')

#see what's in your working directory
dir()

#to work with data files, you must read in your file as a dataframe object
#data <- as.data.frame(read.csv("filename.csv")) #if the file is an xls or xlsx use read.xls
#this dataset is simulated data, so everything is arbitrary
data <- as.data.frame(read.delim('data.dat',header = F, sep = ""))

#view dataset properties
names(data)
head(data)
nrow(data)
View(data)

#how do I index a data frame?
data[1,1]

#add a generated group column to the data
set.seed(012874) #setting a randomization seed so we all get the same "random" list
a = sample(2,300,TRUE) #create a vector with 300 sampled numbers 1 and 2
data$group <- a #assign a new column "group" the vector "a"
levels(data$group)

#must make variable "group" a factor variable (telling R that it's categorical)
data$group <- as.factor(data$group)

#now you should see the levels of your "group" variable as 1, 2
levels(data$group)

## Subsetting data frames
data_1 <-subset(data,data$group ==1)
data_2 <-subset(data,data$group ==2)

#another way to subset the data?
datax <- data$group[data$group==1]

#save new data frames as .csv file in your directory
write.csv(data_1, 'data_1.csv')

## Describing and Displaying Data
#summary statistics
describe(data)

#what does this tell us?
#characteristics of our variables such as mean, median, standard deviation, skewness, etc. 

#histograms, examining variable distributions
#gauge for normal distribution of variables
#using ggplot2
hist1 <-ggplot(data, aes(x=V1)) +
  geom_histogram(position="identity", alpha=0.5, binwidth = 1,color="#e9ecef", fill="#69b3a2") + 
  ggtitle("Distribution of V1")+ theme_classic()

#make a histogram for another variable

#using ggpubr package
hist_group <- gghistogram(data, x = "V1",
            add = "mean", rug = TRUE,
            color = "group", fill = "group",
            palette = "set2")

#boxplot, view distribution of V1 in each gender
box1 <- ggboxplot(data, x = "group", y = "V1",
                  color = "group", palette = "set2", xlab = "group", ylab= "V1")

#make a boxplot for another variable

#barplot
bar1 <- ggplot(data, aes(x=group, y=V1, fill = group)) +
  geom_bar(stat="identity")

# ggplot2 info: http://www.sthda.com/english/wiki/ggplot2-essentials
# ggpubr info: https://rpkgs.datanovia.com/ggpubr/



