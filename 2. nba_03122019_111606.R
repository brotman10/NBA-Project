##### Importing dataset and packages ##### 
rm(list = ls())

install.packages("dplyr")
library(dplyr)

install.packages("animation")
library(animation)

install.packages("randomForest")
library(randomForest)

install.packages("readxl")
library("readxl")

nba = read_xlsx(file.choose())

attach(nba)
nba = nba%>%filter(MP>48)
nba = na.omit(nba)

View(nba)

##### Summary statistics ##### 
glimpse(nba)
summary(nba)
str(nba)

##### Filtering players based on their positions ############

nba_labels=nba[,c(1:8)]
nba_off=nba[,c(1,2,9:21,24,27,29,31:34,37,40,42,46)]
nba_def=nba[,c(1,2,22,23,25,26,28,35,36,38,39,43,47)]
nba_ovr=nba[,c(1,2,23,30,36,41,44,45,48,49)]

nba_C = nba%>%filter(Pos=='C')
nba_off_C = nba_off%>%filter(Pos=='C')
nba_def_C = nba_def%>%filter(Pos=='C')
nba_ovr_C = nba_ovr%>%filter(Pos=='C')

nba_PF = nba%>%filter(Pos=='PF')
nba_off_PF = nba_off%>%filter(Pos=='PF')
nba_def_PF = nba_def%>%filter(Pos=='PF')
nba_ovr_PF = nba_ovr%>%filter(Pos=='PF')

nba_SF = nba%>%filter(Pos=='SF')
nba_off_SF = nba_off%>%filter(Pos=='SF')
nba_def_SF = nba_def%>%filter(Pos=='SF')
nba_ovr_SF = nba_ovr%>%filter(Pos=='SF')

nba_SG = nba%>%filter(Pos=='SG')
nba_off_SG = nba_off%>%filter(Pos=='SG')
nba_def_SG = nba_def%>%filter(Pos=='SG')
nba_ovr_SG = nba_ovr%>%filter(Pos=='SG')

nba_PG = nba%>%filter(Pos=='PG')
nba_off_PG = nba_off%>%filter(Pos=='PG')
nba_def_PG = nba_def%>%filter(Pos=='PG')
nba_ovr_PG = nba_ovr%>%filter(Pos=='PG')

###### Centers Analysis -- Clusters ###### 

##### Offensive Analysis on Centers

##### OWS & OBPM for Centers
nba_off_C1= nba_off_C[,c(25,26)]
rownames(nba_off_C1)=nba_off_C$Player
plot(nba_off_C1)

km.2=kmeans(nba_off_C1,2)
km.3=kmeans(nba_off_C1,3)
km.4=kmeans(nba_off_C1,4)
km.5=kmeans(nba_off_C1,5)
plot(nba_off_C1,col=(km.2$cluster))
plot(nba_off_C1,col=(km.3$cluster))
plot(nba_off_C1,col=(km.4$cluster))
plot(nba_off_C1,col=(km.5$cluster))

kmeans.ani(nba_off_C1, centers = 4, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_off_C1),km.4$cluster)
View(clusters)

##### OWS & FT_Per
nba_off_C1= nba_off_C[,c(14,25)]
rownames(nba_off_C1)=nba_off_C$Player
plot(nba_off_C1)

km.2=kmeans(nba_off_C1,2)
km.3=kmeans(nba_off_C1,3)
km.4=kmeans(nba_off_C1,4)
km.5=kmeans(nba_off_C1,5)
plot(nba_off_C1,col=(km.2$cluster))
plot(nba_off_C1,col=(km.3$cluster))
plot(nba_off_C1,col=(km.4$cluster))
plot(nba_off_C1,col=(km.5$cluster))


kmeans.ani(nba_off_C1, centers = 4, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_off_C1),km.6$cluster)
View(clusters)

##### BMS & VORP For Centers
nba_ovr_C1= nba_ovr_C[,c(9,10)]
rownames(nba_ovr_C1)=nba_ovr_C$Player
plot(nba_ovr_C1)

km.2=kmeans(nba_ovr_C1,2)
km.3=kmeans(nba_ovr_C1,3)
km.4=kmeans(nba_ovr_C1,4)
km.5=kmeans(nba_ovr_C1,5)
plot(nba_ovr_C1,col=(km.2$cluster))
plot(nba_ovr_C1,col=(km.3$cluster))
plot(nba_ovr_C1,col=(km.4$cluster))
plot(nba_ovr_C1,col=(km.5$cluster))

kmeans.ani(nba_ovr_C1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_ovr_C1),km.5$cluster)
View(clusters)

##### VORP & PER for Centers
nba_ovr_C1= nba_ovr_C[,c(4,10)]
rownames(nba_ovr_C1)=nba_ovr_C$Player
plot(nba_ovr_C1)

km.2=kmeans(nba_ovr_C1,2)
km.3=kmeans(nba_ovr_C1,3)
km.4=kmeans(nba_ovr_C1,4)
km.5=kmeans(nba_ovr_C1,5)
plot(nba_ovr_C1,col=(km.2$cluster))
plot(nba_ovr_C1,col=(km.3$cluster))
plot(nba_ovr_C1,col=(km.4$cluster))
plot(nba_ovr_C1,col=(km.5$cluster))

kmeans.ani(nba_ovr_C1, centers = 4, pch = 1:4, col = 1:4,)
clusters=data.frame(rownames(nba_ovr_C1),km.4$cluster)
View(clusters)

##### WS_Game & VORP for Centers
nba_ovr_C1= nba_ovr_C[,c(8,10)]
rownames(nba_ovr_C1)=nba_ovr_C$Player
plot(nba_ovr_C1)

km.2=kmeans(nba_ovr_C1,2)
km.3=kmeans(nba_ovr_C1,3)
km.4=kmeans(nba_ovr_C1,4)
km.5=kmeans(nba_ovr_C1,5)
plot(nba_ovr_C1,col=(km.2$cluster))
plot(nba_ovr_C1,col=(km.3$cluster))
plot(nba_ovr_C1,col=(km.4$cluster))
plot(nba_ovr_C1,col=(km.5$cluster))

kmeans.ani(nba_ovr_C1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_ovr_C1),km.5$cluster)
View(clusters)

## Defensive Analysis on Centers

##### DWS & STL for Centers
nba_def_C1= nba_def_C[,c(5,12)]
rownames(nba_def_C1)=nba_def_C$Player
plot(nba_def_C1)

km.2=kmeans(nba_def_C1,2)
km.3=kmeans(nba_def_C1,3)
km.4=kmeans(nba_def_C1,4)
km.5=kmeans(nba_def_C1,5)
plot(nba_def_C1,col=(km.2$cluster))
plot(nba_def_C1,col=(km.3$cluster))
plot(nba_def_C1,col=(km.4$cluster))
plot(nba_def_C1,col=(km.5$cluster))

kmeans.ani(nba_def_C1, centers = 4, pch = 1:4, col = 1:4,)
clusters=data.frame(rownames(nba_def_C1),km.4$cluster)
View(clusters)

##### DWS & BLK for Centers
nba_def_C1= nba_def_C[,c(6,12)]
rownames(nba_def_C1)=nba_def_C$Player
plot(nba_def_C1)

km.2=kmeans(nba_def_C1,2)
km.3=kmeans(nba_def_C1,3)
km.4=kmeans(nba_def_C1,4)
km.5=kmeans(nba_def_C1,5)
plot(nba_def_C1,col=(km.2$cluster))
plot(nba_def_C1,col=(km.3$cluster))
plot(nba_def_C1,col=(km.4$cluster))
plot(nba_def_C1,col=(km.5$cluster))

kmeans.ani(nba_def_C1, centers = 4, pch = 1:4, col = 1:4,)
clusters=data.frame(rownames(nba_def_C1),km.4$cluster)
View(clusters)

##### VORP & TRB_Per for Centers
nba_ovr_C1= nba_ovr_C[,c(8,10)]
rownames(nba_ovr_C1)=nba_ovr_C$Player
plot(nba_ovr_C1)

km.2=kmeans(nba_ovr_C1,2)
km.3=kmeans(nba_ovr_C1,3)
km.4=kmeans(nba_ovr_C1,4)
km.5=kmeans(nba_ovr_C1,5)
plot(nba_ovr_C1,col=(km.2$cluster))
plot(nba_ovr_C1,col=(km.3$cluster))
plot(nba_ovr_C1,col=(km.4$cluster))
plot(nba_ovr_C1,col=(km.5$cluster))

kmeans.ani(nba_ovr_C1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_ovr_C1),km.5$cluster)
View(clusters)


##### Additonal Visualizations ##### 

install.packages("ggplot2")
library("ggplot2")

## Age(X) with respect to Salary(Y)
ggplot(data = nba, aes(x = nba$Age, y = nba$SalaryUSD)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple")

## Age(X) with respect to Salary(Y)
ggplot(data = nba, aes(x = nba$Age, y = nba$VORP)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple")

## Salary(X) with respect to VORP(Y)
ggplot(data = nba, aes(x = nba$SalaryUSD, y = nba$VORP)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple", formula = y~poly(x,3))

## Field Goals Percentage (X) with respect to Salary(Y)
ggplot(data = nba, aes(x = FG_Per, y = SalaryUSD)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple"

## PTS(X) with respect to Salary(Y)
ggplot(data = nba, aes(x = nba$PTS, y = SalaryUSD)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple", formula = y~poly(x,4))

## FT%(X) with respect to Salary(Y)
ggplot(data = nba, aes(x = FT_Per, y = SalaryUSD)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple")

## Minutes Played(X) with respect to Salary(Y)
ggplot(data = nba, aes(x = nba$MP, y = SalaryUSD)) +
  geom_point() + geom_smooth(method='lm', formula = y~poly(x,2), color = "red", fill = "purple")

## Minutes Played(X) with respect to VORP(Y)
ggplot(data = nba, aes(x = nba$MP, y = VORP)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple")

## TOV_Per(X) with respect to VORP(Y)
ggplot(data = nba, aes(x = nba$TOV_Per, y = VORP)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple")

## OWS(X) with respect to VORP(Y)
fig_ows <- ggplot(data = nba, aes(x = nba$OWS, y = VORP)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple")

## DWS(X) with respect to VORP(Y)
fig_dws <- ggplot(data = nba, aes(x = nba$DWS, y = VORP)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple")

## OBPM(X) with respect to VORP(Y)
fig_obpm <- ggplot(data = nba, aes(x = nba$OBPM, y = VORP)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple")

## DBPM(X) with respect to VORP(Y)
fig_dbpm <- ggplot(data = nba, aes(x = nba$DBPM, y = VORP)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple")


#### combining the above graph
install.packages("magrittr")
library(magrittr)

install.packages("ggpubr")
library(ggplot2)
library(ggpubr)

ggarrange(fig_ows,fig_dws,fig_obpm,fig_dbpm, labels = c("OWS and VORP", "DWS and VORP","OBPM and VORP", "DBPM and VORP"))


## FT%(X) with respect to Salary(Y) -- grouped by players that made ft_percentage above 70%
# Ellipse by groups
a <- ggplot(nba, aes(FT_Per, SalaryUSD, color = FT_Per > 0.7))+
  geom_point()
a + stat_ellipse()
# Change the type of ellipses: possible values are "t", "norm", "euclid"
a + stat_ellipse(type = "norm")

## Visualizing VORP and SalaryUSD
b <- ggplot(nba, aes(ThreeP_Per, SalaryUSD, color =  ThreeP_Per> 0.4))+
  geom_point()
b + stat_ellipse()
# Change the type of ellipses: possible values are "t", "norm", "euclid"
b + stat_ellipse(type = "norm")

## Visualizing VORP and SalaryUSD
c <- ggplot(nba, aes(VORP, SalaryUSD, color = VORP > 1))+
  geom_point()
c + stat_ellipse()
# Change the type of ellipses: possible values are "t", "norm", "euclid"
c + stat_ellipse(type = "norm")

## Visualizing Three point percentage and VORP
## You can have a high VORP with a low 3pt percentage
d <- ggplot(nba, aes(ThreeP_Per, VORP, color = VORP > 1))+
  geom_point()
d + stat_ellipse()
# Change the type of ellipses: possible values are "t", "norm", "euclid"
d + stat_ellipse(type = "norm")

## Visualizing VORP and free throw percentage
e <- ggplot(nba, aes(VORP, FT_Per, color = FT_Per > 0.85))+
  geom_point()
e + stat_ellipse()
# Change the type of ellipses: possible values are "t", "norm", "euclid"
e + stat_ellipse(type = "norm")


##### Random Forest #####

forest1=randomForest(SalaryUSD~FG_Per+ThreeP_Per+TwoP_Per, ntree=500, data=nba_C, importance=TRUE)
importance(forest1)

forest2=randomForest(SalaryUSD~FG_Per+ThreeP_Per+TwoP_Per+FT_Per, ntree=500, data=nba_C, importance=TRUE)
importance(forest2)

forest3=randomForest(SalaryUSD~FG_Per+ThreeP_Per+TwoP_Per+FT_Per+ORB+DRB, ntree=500, data=nba_C, importance=TRUE)
importance(forest3)

forest4=randomForest(SalaryUSD~FG_Per+ThreeP_Per+TwoP_Per+FT_Per+AST+STL+BLK+TOV+PF, ntree=500, data=nba_C, importance=TRUE)
importance(forest4)
