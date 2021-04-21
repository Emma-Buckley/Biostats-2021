#Emma Buckley
#21 April 2021
#Linear Regressions
#Day 3

#Loading tidyverse

library(tidyverse)

#Getting data faithful

data("faithful")

#First six rows of faithful

head(faithful)

#Creating a sub dataframe called eruption.lm.
#fit the model in R. When we perform a linear regression in R, it will 
#output the model and the coefficients

eruption.lm <- lm(eruptions ~ waiting, data = faithful)

#summarising the dataframe
summary(eruption.lm)

slope <- round(eruption.lm$coef[2], 3)

# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0.
p.val = 0.001

r2 <- round(summary(eruption.lm)$r.squared, 3)

#Creating a ggplot

ggplot(data = faithful, aes(x = waiting, y = eruptions)) + #creating a plot with points
  geom_point() +#point plot
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +#adding text to the graph
  stat_smooth(method = "lm", colour = "red") +
  labs(title = "Old Faithful eruption data", #adding labels
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")
