#Emma Buckley
#Day_2
#Quiz
#20 April 2021

#Question 1:List different classes of data
#1. Numerical data:
#1.1 Discrete data - These are whole numbers and an example is 1.
#1.2 Continuous data - These would be numbers that are not whole numbers, such as 
#decimal numbers, an example is 0.1.
#1.3 Dates - such as 20 April 2021

#2. Qualitative data:
#2.1 Categorical data - These are dats with Categories such as colour or 
#  type of flower
#2.2 ordinal data - These are data with rankings - high, middle, low

#3. Binary data - These are data where there are only two outcomes 
#   such as true and false
#4. Complex numbers - Numbers that are complex
#5. Missing values - Missing values will be N/A

#Functions to view your data:
#View(dataset)
#summary(dataset)
#head(dataset)
#tail(dataset)
#glimpse(dataset)

#Discuss skewness and Kurtosis:
#Skewness:
#There are two types of skewness:
#Positively skewed and negatively skewed
#Positively skewed is when the data is skewed to the right and Negatively skewed
#data is skewed to the left


#Kurtosis:
#There are three outcomes from Kurtosis:
#Mesokurtic, Leprokurtic and Platykurtic
#Mesokurtic is when the data looks normally distributed
#Leprokurtic is when the data has a sharp increase and decrease.
#Platykurtic is when the data has a flat curve with a gradual increase and decrease.

#Question 2:

#Loading the tidyverse package
library(tidyverse)
library(e1071)
data()
ls("package:datasets")
#Loading the dataset orange
Orange <- datasets::Orange
View(Orange)
str(Orange)
#The variable tree is ordinal data and therefore belongs to the Qualitative data
#The variable age is numerical data and the type of numerical data is discrete data
#The variable circumference is numerical data and the type of numerical data is discrete data
#The dataset Orange is numerical data

#First six rows:
head(Orange)
#Last six rows:
tail(Orange)
#column names:
colnames(Orange)
#Summary of the statistics:
summary(Orange)

#Mean, median and Standard deviation of age and circumference for each trees

#Mean for age
Orange %>% 
  group_by(Tree) %>%
  summarise(Orange = mean(age))

#Median for age
Orange %>% 
  group_by(Tree) %>%
  summarise(Orange = median(age))

#Standard deviation for age
Orange %>% 
  group_by(Tree) %>% 
  summarise(Orange = sd(age))

#Mean for circumference
Orange %>% 
  group_by(Tree) %>%
  summarise(Orange = mean(circumference))

#Median for circumference:
Orange %>% 
  group_by(Tree) %>%
  summarise(Orange = median(circumference))

#Standard deviation for circumference:
Orange %>% 
  group_by(Tree) %>% 
  summarise(Orange = sd(circumference))


library(e1071)
kurtosis(Orange)

#Skewness:  
skewness(Orange)
  
#summarise to determine min, max, quartiles for circumference  
Orange %>% 
  group_by(Tree) %>%
  summarise(min_wt = min(circumference),
            qrt1_wt = quantile(circumference, p = 0.25),
            med_wt = median(circumference),
            qrt3_wt = quantile(circumference, p = 0.75),
            max_wt = max(circumference))  

#Creating plots:
#plot 1:
ggplot(data = Orange, aes(x = Tree, y = circumference)) +
  geom_point()+
  labs(x = "Trees", y = "Circumference",
       title = "The circumference of the different trees")

#plot 2:
ggplot(data = Orange, aes(x = age, y = circumference)) +
  geom_point()+
  labs(x = "Ages", y = "Circumference",
       title = "The ages vs circumference of the different trees")
              
#Question 3:
#Explain the following functions
#The function mutate() creates new columns 
#The function select() selects certain columns of your choice
#The function group_by() groups the dataset by a specific variable of choice
#The function filter() selects certain rows within your dataset
#The function separate() separates a column into multiple columns

