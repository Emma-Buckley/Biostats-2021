#Biostats
#Emma Buckley
#Day 1
#Viewing data and Basic Stats
#19 April 2021
#Part 1

?BOD #to see info about the data
BOD #view the data
str(BOD) #to see the types of data
summary(BOD)

str(InsectSprays)
View(InsectSprays)
unique(InsectSprays$spray)# to see the different levels of spray

?Loblolly
str(Loblolly)

?HairEyeColor
HairEyeColor
str(HairEyeColor)

Seatbelts
str(Seatbelts)
View(Seatbelts)
?Seatbelts

cars
str(cars)

?esoph
str(esoph)

?JohnsonJohnson
str(JohnsonJohnson)
View(JohnsonJohnson)

?volcano
str(volcano)
View(volcano)


#Load built-in data
pines <- Loblolly
str(pines) #structure of the data
class(pines$height)#what type of class a particular column is

#Part 2:

#calculate sample size
library(tidyverse)
chicks <- as_tibble(ChickWeight)
chicks
nrow(chicks)
?ChickWeight
unique(chicks$Chick) #sample size

#note the distinction between 'nrows' and the 'true' sample size
nrow(chicks)
unique(chicks$Chick)#counts no. of unique levels

#calculate the mean weight of all chickens at day 20
library(tidyverse)
view(chicks)
chicks %>% 
  filter(Time == 20) %>% 
  summarise(chicks = mean(weight))

#calculate the mean weight for each diets
library(tidyverse)
view(chicks)
chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) %>% 
  summarise(chicks = mean(weight))

#sd for all chicks
library(tidyverse)
view(chicks)
chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) %>% 
  summarise(chicks = mean(weight),
            chicks = sd(weight))

#mean, median, std per diet

chicks %>% 
  filter(Time == 20) %>% 
  group_by(Diet) %>% 
  summarise(chicks = mean(weight),
            chicks = sd(weight),
            chicks = median(weight))

#Kurtosis
install.packages("e1071")
library(e1071)


chicks %>% 
  group_by(Diet) %>%
  filter(Time == 20) %>% 
  summarise(kurtosis = kurtosis(weight))

chicks %>% 
  summarise(mean_wt = mean(weight))            


chicks %>% 
  group_by(Diet) %>%
  filter(Time == 20) %>% 
  summarise(min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = quantile(weight, p = 0.75),
            max_wt = max(weight))

#range of the chicks
chicks %>% 
  summarise(lower_wt = range(weight)[1],
            upper_wt = range(weight)[2])

chicks %>% 
  group_by(Diet) %>%
  filter(Time == 20) %>% 
  summarise(lower_wt = range(weight)[1],
            upper_wt = range(weight)[2])


dat1 <- c(NA, 12, 76, 34, 23)
mean(dat1)
mean(dat1, na.rm = TRUE)#to remove the missing value

