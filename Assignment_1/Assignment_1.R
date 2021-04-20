#Biostatistic and Intro_R recap assignment: 
#Data Manipulation, Anlyses and Visualisation
#20 April 2021
#Emma Buckley

#Install packages:
install.packages("dslabs")
#Load the necessary packages:
library(tidyverse)
library(ggplot2)

# Section 1 ---------------------------------------------------------------
BOD
?BOD

#Answer: C


# Section 2 ---------------------------------------------------------------

library(dplyr)
library(dslabs)
data(murders)

#Exploring the murders dataset:

glimpse(murders)
head(murders)
tail(murders)
view(murders)

?murders
#Describing murder data:

#The murder data shows the United States (US) gun murders by each state for 2010.
#The gun murder data has been collected from FBI reports and it also contains 
#the population in each state. The first column is the different US states, the 
#second column abbreviates the US state. The third column is the geographical 
#region within the US, the fourth column is the population of people in 
#each state in 2010.The fifth column is the number of gun murders within the 
#state in 2010.

#Using the select function to only show the states and population size

murders %>% 
  select(c(state, population))

#Remove Florida from the dataset

murders %>% 
  filter(state != "Florida")

#Remove South from the states region from the dataset
no_south <- murders %>% 
  filter(region != "South")

#There are 34 states in the no south dataset.

#Calculate the population size of the South and West regionally

#Population size for the South: 115,674,434

murders %>% 
  filter(region == "South") %>% 
  summarise(pop_size_South = sum(population))

#Population size for the West: 71,945,553

murders %>% 
  filter(region == "West") %>% 
  summarise(pop_size_West = sum(population))

#Creating data frame with only population size of the Northeast region
no_northeast <- murders %>%
  filter(region == "Northeast") %>% 
  summarise(pop_size_Northeast = sum(population))

#Create two plots of your choice and explain visible trends

#Plot 1:

murders %>% 
  select(state, total) %>%
  ggplot(aes(x = state, y = total)) +
  geom_point() +
  labs(x = "State", y = "Total murders",
       title = "The total number of murders per state") +
  theme_bw() +
  theme(axis.text = element_text(angle = 45, hjust = 1))

#Trends for plot 1:
#This graph shows the total number of murders per state.
#California has the highest number of total gun murders, followed by Texas 
#and then Florida. Most of the total murders are between 0 and 400.

#Plot 2:

murders %>% 
  select(region, total) %>%
  ggplot(aes(x = region, y = total)) +
  geom_boxplot(outlier.colour = "red") +
  labs(x = "Regions", y = "Total murders",
       title = "The total number of murders per region") +
  theme_bw()

#Trends for plot 2:
#This graph shows the total number of murders per region. The South and West
#have two outliers and the North East and North Central do not have outliers.
#The North Central has the greatest number of gun murders, followed by the
#North East. The West region has the smallest number of murders.


#Compare with population size of the South with the population size of the West

murders %>% 
  filter(region %in% c("South", "West")) %>% 
  arrange(region) %>% 
  summarise(pop_south = sum(population[c(1:17)]),
            pop_west = sum(population[c(18:30)]))

#The South has a population size of 115,674,434 and has 17 states
#The West has a population size of 71945553 and has 13 states

#Create a new data frame where the total>20 but <100

murders_20_100 <- murders %>% 
  filter(total > 20 & total < 100)
 
#Create an object, containing from 10th to 24th row and 26th row

murders_10_26 <- murders%>% 
  slice(10:24, 26)

#Create a tibble table

murders_tibble <-as_tibble(murders)

#convert murders into a tibble that is grouped by region

murders %>% 
  as_tibble() %>% 
  group_by(region)

# Section 3 ---------------------------------------------------------------

library(dplyr)
library(dslabs)
data(heights)  
  
?heights
#Describing height data:

#The height data shows the different heights of males and females in inches.
#The heights are self-reported.The first column is the sex and distinguishes 
#between a male and female. The second column is the height of the individual
#in inches.

#Explore the datasets
glimpse(heights)
head(heights)
tail(heights)
view(heights)

#Determine the average and standard deviation for males and females.
#Then calculate the median, minimum and maximum values

heights %>% 
  group_by(sex) %>% 
  summarise(average = mean(height),
            sd = sd(height),
            minimum = min(height),
            maximum = max (height),
            median = median(height))


# Section 4 ---------------------------------------------------------------

x <- c( 1, 6, 21, 19 , NA, 73, NA)
y <- c(NA, NA, 3, NA, 13, 24, NA)

x
#a Count the number of elements are missing in both x and y:

sum(is.na(x))
sum(is.na(y))

#b: Transform the code, used above (a), into a function:

Missing_values <- function(x,y,z) {
  sum_NA_x <- sum(is.na(x))
  sum_NA_y <- sum(is.na(y))
  sum_NA_z <- sum(is.na(z))
  answer <- c(sum_NA_x, sum_NA_y, sum_NA_z)
  return(answer)
}

#C
a <- c(NA, 12, 52, 66, NA, 34, 56)
b <- c(22, 51, NA, NA, NA, NA, 76)
c <- c(34, 78, 99, NA, 56, NA, 77)
Missing_values(a, b, c)

# Section 5 ---------------------------------------------------------------

#input the data

Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),
                            winter = c(41, 39, 47, 40),
                            spring = c(41, 46, 57, 45),
                            summer = c(75, 52, 85, 66),
                            Autumn = c(57, 66, 52, 56))
#Design an hypothesis:

#Hypothesis: The values for summer will be higher than the other seasons.

#Tidy the data
Seasonal_data_tidy <- Seasonal_data %>% 
  gather(winter,summer,spring,Autumn, key = "season", value = "Value")

#Create two plots:

#Plot 1:

ggplot(data = Seasonal_data_tidy, aes(x = season, y = Value)) +
  geom_boxplot()+
  labs(x = "Season", y = "Value",
       title = "The values for the different seasons") +
  theme_bw()

#Plot 2:

ggplot(data = Seasonal_data_tidy, aes(x = year, y = Value)) +
  geom_point()+
  labs(x = "Year", y = "Value",
       title = "The values for the different years") +
  theme_bw()

#write a paragraph discussing your findings:

#These findings show that the values for summer are higher than the 
#other seasons.The values for 2017 are higher than the other years.


cats_data<- tibble(cats = c("A", "B", "C"),
                   position = c("1-2-3", "3-1-2", "2-3-1"),
                   minutes = c(3, 3, 3),
                   seconds = c(12, 44, 15))
cats_data

#Split the position column into new three columns

cats_data %>% 
  separate(position, c("first_place", "second_place", "third_place"))

#Uniting the minutes and seconds column into its own column

cats_data %>% 
  unite("minutes", "seconds", col = total_time, sep = ":")


# Section 6 ---------------------------------------------------------------

data("ToothGrowth")

#Using the ToothGrowth data, I am grouping the data by dose and supplement
#and the finding the mean length for each of the doses and supplements.

ToothGrowth %>% 
  group_by(dose, supp) %>% 
  summarise(mean_length = mean(len))

#Selecting only the dose column and arranging it in ascending order

ToothGrowth %>%
  select(dose) %>% 
  arrange(dose) 

#This is first joining the column supp and dose into one column called supp_and_
#dose

toothgrowth_unite <- ToothGrowth %>% 
  unite(supp, dose, col = "supp_and_dose", sep = "/") 

#This is creating a dataset separating the column supp_and_dose into two columns

toothgrowth_sep <- toothgrowth_unite %>% 
  separate(col = supp_and_dose, into = c("supp", "dose"), sep = "/")

#Joining the dataset toothgrowth_sep with toothgrowth_unite dataset

tooth_join <- left_join(toothgrowth_sep, toothgrowth_unite) 

#This is creating a dataset called tooth_mutate from Tooth Growth and 
#creating a new column named len_1. The len_1 is the length plus 1.

tooth_mutate <- ToothGrowth %>% 
  mutate(len_1 = len + 1)

#This is creating a dataset by gathering data and creating one column for 
#the variable and the other for the value of the variable.The supplements 
#and lengths are under the variable column. The values that correspond to the 
#supplements and length are under the value column.

toothgrowth_gather <- ToothGrowth %>% 
  gather(supp, len, key = "variable", value = "value")   

#We are creating a new data frame from Toothgrowth and spreading the supp column
#and using the lens as the values. This code gave me an error message as the 
#two rows contain the same values and therefore cannot be spreaded.
#ToothGrowth data is not the best example for using spread.

tooth_spread <- ToothGrowth %>% 
  spread(key = supp, value = len)

#I chose another set of data called fish_encounters which works 
#and can be spreaded.This shows you I can spread data.

data("fish_encounters")
fish_e_spread <- fish_encounters %>% 
  spread(key = station, value = seen)

        
  