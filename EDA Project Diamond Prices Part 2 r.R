#load ggplot 2 library - diamonds data set is part of this
library(ggplot2)

#1. Create a scatterplot of price vs x using the ggplot syntax
ggplot(aes(x = x, y = price), data = diamonds) +
  geom_point()

#2. What are your observations about this scatterplot?
#Exponential relationship with a few outliers

#3. What is the corelation between
#price and x: 0.88
cor.test(diamonds$price, diamonds$x)
#price and y: 0.87
cor.test(diamonds$price, diamonds$y)
#Price and z: 0.86
cor.test(diamonds$price, diamonds$z)

#4. Create a simple scatter plot of price vs depth
ggplot(aes(x = depth, y = price), data = diamonds) +
  geom_point()

#5. Change the code to make the transparency of the points to be 1/100 of what they are 
#now and mark the x-axis every 2 units
ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks = seq(0,80,2))

#6. Based on the price vs depth scatterplot, most diamonds are between what depths?
#59 - 64

#7. What is the correlation of depth vs price? -0.01
cor.test(diamonds$depth, diamonds$price)
#Would you use depth to predict price of a diamond? No
#why? coefficient indicates no correlation between the two

#8. Create a scatterplot of price vs carat and omit the top 1% of price and carat values
ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point() +
  xlim(0, quantile(diamonds$carat, 0.99)) +
  ylim(0, quantile(diamonds$price, 0.99))

#9. Create a scatterplot of price vs. volume (x * y * z).This is a very rough approximation 
#for a diamond's volume. Create a new variable for volume in the diamonds data frame.
#This will be useful in a later exercise.
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(aes(x = volume, y = price), data = diamonds) + 
  geom_point()

#10. What are your observations of the price vs volume scatterplot?
#very steep linear correlation with a few outliers, and a few null values for volume

#11. What is the correlation of price and volume? Exclude diamonds with a volume of zero, 
#or are greater than or equal to 800 : 0.92
with(subset(diamonds, volume > 0 & volume < 800), cor.test(volume, price))

# 12. Subset the data to exclude diamonds with a volume greater than or equal to 800. 
# Also, exclude diamonds with a volume of 0. 
# Adjust the transparency of the points and add a linear model to the plot. 
# Do you think this would be a useful model to estimate the price of diamonds? 
#Why or why not? yes - high correlation.

diamonds_subset <- subset(diamonds, volume > 0 & volume < 800)
ggplot(aes(x = volume, y = price), data = diamonds_subset) + 
  ylim(0, 20000) +
  geom_point(alpha = 1/20) +
  geom_smooth(method = "lm", color = "blue")

#13. Use the function dplyr package to create a new data frame containing 
#info on diamonds by clarity. Name the data frame diamondsByClarity. 
#The data frame should contain the following variables in this order:
#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n
# Where n is the number of diamonds in each level of clarity?
library(dplyr)
diamondsByClarity <- diamonds %>%
  group_by(clarity) %>%
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price),
            n = n()) %>%
  arrange(clarity)

#14. We've created summary data frames with the mean price by clarity and color. 
# You can run the code in R to verify what data is in the variables diamonds_mp_by_clarity
# and diamonds_mp_by_color.

# Your task is to write additional code to create two bar plots on one output image 
# using the grid.arrange() function from the package gridExtra.

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

library(gridExtra)
grid.arrange(
ggplot(aes(x = diamonds_mp_by_clarity$clarity, y = diamonds_mp_by_clarity$mean_price), 
       data = diamonds_mp_by_clarity) +
  geom_bar(stat = "identity"),
ggplot(aes(x = diamonds_mp_by_color$color, y = diamonds_mp_by_color$mean_price), 
       data = diamonds_mp_by_color) +
  geom_bar(stat = "identity")
)

#15. What do you notice in each of the bar charts?
#relationship between clarity and mean price, and color and mean price is not as expected. 
#would expect to see higher means for better color and clarity

#16. Examine pairs of variable and create 2-5 plots that make 
#use of the techniques from Lesson 4.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. the variable(s) you investigated, your observations, and any summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots

#use economics data in ggplot
#scatter plot comparing number of unemployed and median duration of unemployment
ggplot(aes(x = unemploy, y = uempmed), data = economics) + 
  geom_point()
#add mean line
ggplot(aes(x = unemploy, y = uempmed), data = economics) + 
  geom_point() +
  geom_smooth(method = "lm")
#check correlation
cor.test(economics$unemploy, economics$uempmed)
#create group
economics.rate <- economics %>%
  group_by(psavert) %>%
  summarise(MeanPcePerPerson = mean(pce/pop),
            MeanUnemRate = mean(unemploy/pop),
            n = n())
#compare means
grid.arrange(
  ggplot(aes(x = MeanPcePerPerson, y = psavert), data = economics.rate) +
    geom_line() +
    geom_smooth(),
  ggplot(aes(x = MeanUnemRate, y = psavert), data = economics.rate) +
    geom_line() +
    geom_smooth(),
  ggplot(aes(x = MeanUnemRate, y = MeanPcePerPerson), data = economics.rate) +
    geom_line() +
    geom_smooth()
)
#confirm correlations
cor.test(economics.rate$MeanPcePerPerson, economics.rate$psavert)
cor.test(economics.rate$MeanUnemRate, economics.rate$psavert)
cor.test(economics.rate$MeanUnemRate, economics.rate$MeanPcePerPerson)


  









