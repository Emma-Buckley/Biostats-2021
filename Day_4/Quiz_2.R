#Emma Buckley
#22 April 2021
#Quiz 2
#Biostats

ls("package:datasets")
library(tidyverse)
library(ggplot2)
Orange <- datasets::Orange
ToothGrowth <- datasets::ToothGrowth
warpbreaks <- datasets::warpbreaks
#Question 1:

#Orange data:
#hypothesis 1: The circumferences different between the different trees:

Orange_1 <- Orange %>% 
  select(Tree, circumference)

#Plot the data:

ggplot(Orange_1) +
  geom_point(aes(x = Tree, y = circumference))

#Normality test:
Orange %>% 
  group_by(Tree) %>% 
  summarise(Orange_norm = as.numeric(shapiro.test(circumference)[2]))

#Homoscedastic: 
Orange %>% 
  group_by(Tree) %>% 
  summarise(circum_var = var(circumference))
library(ggpubr)


#This does the normality test and the Shapiro test at the same time
two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

Orange_1 %>% 
  group_by(Tree) %>% 
  summarise(tree_var = two_assum(circumference)[1],
            tree_norm = two_assum(circumference)[2])

#ANOVA test:
Orange.aov1 <- aov(circumference ~ Tree, data = Orange_1)
summary(Orange.aov1)

TukeyHSD(Orange.aov1)
plot(Orange.aov1)

#Correlation:
Orange_2 <- Orange %>% 
  select(age, circumference)

cor.test(x = Orange_2$age, Orange_2$circumference, #specifying a column
         use = "everything", method = "pearson")

#This shows that the age and circumference of the trees are strongly correlated
#My hypothesis is true, that each tree is different.


#ToothGrowth data:

#hypothesis: The orange juice was a better delivery method
#Plot the data:
tooth_growth_1 <- ToothGrowth %>% 
  select(supp, len)

ggplot(data = tooth_growth_1) +
  geom_col(aes(x = supp, y = len ))

#Normality test:
tooth_growth_1 %>% 
  group_by(supp) %>% 
  summarise(tooth_norm = as.numeric(shapiro.test(len)[2]))

#homoscedasity
tooth_growth_1 %>% 
  group_by(supp) %>% 
  summarise(supp_var = var(len))


t.test(len ~ supp, data = tooth_growth_1, var.equal = TRUE)

tooth_growth2 <- ToothGrowth %>% 
  select(dose, len)

#Correlation test:
#The correlation between the length and the dose is more correlated as the value
#is 0.8. This shows a high correlation between the two.
cor.test(x = tooth_growth2$dose, tooth_growth_1$len, #specifying a column
         use = "everything", method = "pearson")

#My hypothesis is true that Orange juice is a better delivery method than the 
#vitamin C

#Wrapbreaks
#The type A wool is better than the type B wool:

#Plot the data:
warp_break_1 <- warpbreaks %>% 
  select(breaks, wool)

ggplot(data = warp_break_1) +
  geom_col(aes(x = wool, y = breaks ))

#Normality test:
warp_break_1 %>% 
  group_by(wool) %>% 
  summarise(wool_norm = as.numeric(shapiro.test(breaks)[2]))

#homoscedasity
warp_break_1 %>% 
  group_by(wool) %>% 
  summarise(wool_var = var(breaks))


#ANOVA test:
warp_break.aov1 <- aov(breaks ~ wool, data = warp_break_1)
summary(warp_break.aov1)

TukeyHSD(warp_break.aov1)
plot(warp_break.aov1)

#Question 2:
load("~/Biostatistics/Second part of R/Biostats-2021/data/SACTN_daily_v4.2.RData")

#Created a new dataframe with the headings site and source
SACTN_tidy <- SACTN_daily_v4.2 %>% 
  separate(col = index, into = c("site", "src"), sep = "/")

SACTN_tidy2 <- SACTN_tidy %>% 
  separate(col = date, into = c("year", "month", "day"))


SACTN_3 <- SACTN_tidy2 %>% 
  select(month, temp)

#This is comparing the temperatures for the different months
ggplot(data = SACTN_3) +
  geom_col(aes(x = month, y = temp), fill = "red") +
  labs(x = "Month", y = "Temperatures")

SACTN_4 <- SACTN_tidy2 %>% 
  select(site, src, month, temp)

SACTN_5 <- SACTN_4 %>% 
  select(site, temp)

ggplot(data = SACTN_5) +
  geom_col(aes(x = site, y = temp), fill = "red") +
  labs(x = "Site", y = "Temperatures")

SACTN_6 <- SACTN_4 %>% 
  select(src, temp)

ggplot(data = SACTN_6) +
  geom_col(aes(x = site, y = temp), fill = "red") +
  labs(x = "Source", y = "Temperatures") 
