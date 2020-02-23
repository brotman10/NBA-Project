library("readxl")

nba = read_xlsx(file.choose())

library(ggplot2)
library(dplyr)

glimpse(nba)
summary(nba) #summary statistics
str(nba) #to understand variable types 

attach(nba)

str(nba)

############    Visualizations ############
## Age(X) with respect to Salary(Y)
ggplot(data = nba, aes(x = nba$Age, y = nba$SalaryUSD)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "orange")

## Field Goals Percentage (X) with respect to Salary(Y)
ggplot(data = nba, aes(x = FG_Per, y = SalaryUSD)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "orange")

## PTS(X) with respect to Salary(Y)
ggplot(data = nba, aes(x = PTS, y = SalaryUSD)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "orange")

## FT%(X) with respect to Salary(Y)
ggplot(data = nba, aes(x = FT_Per, y = SalaryUSD)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "orange")

## FT%(X) with respect to Salary(Y) -- grouped by players that made ft_percentage above 70%

# Ellipse by groups
p <- ggplot(nba, aes(FT_Per, SalaryUSD, color = FT_Per > 0.7))+
  geom_point()
p + stat_ellipse()
# Change the type of ellipses: possible values are "t", "norm", "euclid"
p + stat_ellipse(type = "norm")

## Visualizing VORP and SalaryUSD
p <- ggplot(nba, aes(ThreeP_Per, SalaryUSD, color = 3pt_percent > 0.4))+
  geom_point()
p + stat_ellipse()
# Change the type of ellipses: possible values are "t", "norm", "euclid"
p + stat_ellipse(type = "norm")

## Visualizing VORP and SalaryUSD
p <- ggplot(nba, aes(VORP, SalaryUSD, color = VORP > 1))+
  geom_point()
p + stat_ellipse()
# Change the type of ellipses: possible values are "t", "norm", "euclid"
p + stat_ellipse(type = "norm")

## Visualizing Three point percentage and VORP
## You can have a high VORP with a low 3pt percentage
p <- ggplot(nba, aes(ThreeP_Per, VORP, color = VORP > 1))+
  geom_point()
p + stat_ellipse()
# Change the type of ellipses: possible values are "t", "norm", "euclid"
p + stat_ellipse(type = "norm")

## Visualizing VORP and free throw percentage
p <- ggplot(nba, aes(VORP, nba$FT_Per, color = VORP > 0))+
  geom_point()
p + stat_ellipse()
# Change the type of ellipses: possible values are "t", "norm", "euclid"
p + stat_ellipse(type = "norm")


