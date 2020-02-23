rm(list = ls())

library(dplyr)
library(animation)
library(randomForest)

attach(nba)
nba = nba%>%filter(MP>48)
nba = na.omit(nba)
glimpse(nba)
summary(nba)
str(nba)

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

##### OWS & OBPM for Centers #####

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

##### BMS & VORP For Centers #####

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

##### Random Forest #####

forest1=randomForest(SalaryUSD~FG_Per+ThreeP_Per+TwoP_Per, ntree=500, data=nba_C, importance=TRUE)
importance(forest1)
forest2=randomForest(SalaryUSD~FG_Per+ThreeP_Per+TwoP_Per+FT_Per, ntree=500, data=nba_C, importance=TRUE)
importance(forest2)
forest3=randomForest(SalaryUSD~FG_Per+ThreeP_Per+TwoP_Per+FT_Per+ORB+DRB, ntree=500, data=nba_C, importance=TRUE)
importance(forest3)
forest4=randomForest(SalaryUSD~FG_Per+ThreeP_Per+TwoP_Per+FT_Per+AST+STL+BLK+TOV+PF, ntree=500, data=nba_C, importance=TRUE)
importance(forest4)
