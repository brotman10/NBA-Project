rm(list = ls())

##### Loading packages and dataset ###### 
library(dplyr)

install.packages("animation")
library(animation)

install.packages("randomForest")
library(randomForest)

install.packages("ggplot2")
library(ggplot2) 

install.packages("ggfortify")
library(ggfortify)

install.packages("gbm")
library(gbm)

install.packages("readxl")
library("readxl")

nba = read_xlsx(file.choose())
attach(nba)

nba = nba%>%filter(MP>49)
nba = na.omit(nba)
glimpse(nba)
summary(nba)
str(nba)

## View the dataset
View(nba)

##### Filtering players based on their positions #####

nba_labels=nba[,c(1:8)]
nba_off=nba[,c(1,2,8:20,23,26,28,30,21,32,34,37)]
nba_off_reg=nba[,c(1,2,8,9:20,23,26,28)]
nba_off_adv=nba[,c(1,2,8,30,31,32,34,37)]
nba_def=nba[,c(1,2,8,21,22,24,25,27,35,38)]
nba_ovr=nba[,c(1,2,8,29,33,36,39,40)]

nba_C = nba%>%filter(Pos=='C')
nba_off_C = nba_off%>%filter(Pos=='C')
nba_off_reg_C = nba_off_reg%>%filter(Pos=='C')
nba_off_adv_C = nba_off_adv%>%filter(Pos=='C')
nba_def_C = nba_def%>%filter(Pos=='C')
nba_ovr_C = nba_ovr%>%filter(Pos=='C')

nba_PF = nba%>%filter(Pos=='PF')
nba_off_PF = nba_off%>%filter(Pos=='PF')
nba_off_reg_PF = nba_off_reg%>%filter(Pos=='PF')
nba_off_adv_PF = nba_off_adv%>%filter(Pos=='PF')
nba_def_PF = nba_def%>%filter(Pos=='PF')
nba_ovr_PF = nba_ovr%>%filter(Pos=='PF')

nba_SF = nba%>%filter(Pos=='SF')
nba_off_SF = nba_off%>%filter(Pos=='SF')
nba_off_reg_SF = nba_off_reg%>%filter(Pos=='SF')
nba_off_adv_SF = nba_off_adv%>%filter(Pos=='SF')
nba_def_SF = nba_def%>%filter(Pos=='SF')
nba_ovr_SF = nba_ovr%>%filter(Pos=='SF')

nba_SG = nba%>%filter(Pos=='SG')
nba_off_SG = nba_off%>%filter(Pos=='SG')
nba_off_reg_SG = nba_off_reg%>%filter(Pos=='SG')
nba_off_adv_SG = nba_off_adv%>%filter(Pos=='SG')
nba_def_SG = nba_def%>%filter(Pos=='SG')
nba_ovr_SG = nba_ovr%>%filter(Pos=='SG')

nba_PG = nba%>%filter(Pos=='PG')
nba_off_PG = nba_off%>%filter(Pos=='PG')
nba_off_reg_PG = nba_off_reg%>%filter(Pos=='PG')
nba_off_adv_PG = nba_off_adv%>%filter(Pos=='PG')
nba_def_PG = nba_def%>%filter(Pos=='PG')
nba_ovr_PG = nba_ovr%>%filter(Pos=='PG')

##### PCA PG #####

nba_off_reg_PG_vars= nba_off_reg_PG[,c(3:18)]
pca_off_reg_PG = prcomp(nba_off_reg_PG_vars,scale=TRUE)
pca_off_reg_PG
autoplot(pca_off_reg_PG, data = nba_off_reg_PG_vars, loadings = TRUE, loadings.label = TRUE)
#TwoP_Per PTS FG_Per FT_Per FT FGA AST 

nba_off_adv_PG_vars = nba_off_adv_PG[,c(3:8)]
pca_off_adv_PG = prcomp(nba_off_adv_PG_vars,scale=TRUE)
pca_off_adv_PG
autoplot(pca_off_adv_PG, data = nba_off_adv_PG_vars, loadings = TRUE, loadings.label = TRUE,labels=Player)
#OBPM TS_Per OWS

nba_def_PG_vars = nba_def_PG[,c(3:10)]
pca_def_PG = prcomp(nba_def_PG_vars,scale=TRUE)
pca_def_PG
autoplot(pca_def_PG, data = nba_def_PG_vars, loadings = TRUE, loadings.label = TRUE)
#DWS DRB STL

nba_ovr_PG_vars = nba_ovr_PG[,c(3:8)]
pca_ovr_PG = prcomp(nba_ovr_PG_vars,scale=TRUE)
pca_ovr_PG
autoplot(pca_ovr_PG, data = nba_ovr_PG_vars, loadings = TRUE, loadings.label = TRUE)
#WS BPM VORP

##### Random Forest PCA Features PG #####

set.seed(1)
forest_PG=randomForest(SalaryUSD~TwoP_Per+FT_Per+PTS+AST+TS_Per+DWS+WS+BPM, ntree=500, data=nba_PG,importance=TRUE)
forest_PG
#drop FG_Per FT VORP OWS from collinearity
#drop DRB STL FGA from importance 

importance(forest_PG)
predicted_salary_PG=predict(forest_PG,data=nba_PG)
predicted_salary_PG
Dif = nba_PG$SalaryUSD-predicted_salary_PG
Salary_PG = data.frame(nba_PG$Player,nba_PG$Tm,predicted_salary_PG,nba_PG$SalaryUSD,Dif)

#do.trace
forest1_PG=randomForest(SalaryUSD~TwoP_Per+FT_Per+PTS+AST+TS_Per+DWS+WS+BPM, ntree=500, do.trace=50,data=nba_PG,importance=TRUE)

##### PCA SG #####

nba_off_reg_SG_vars= nba_off_reg_SG[,c(3:18)]
pca_off_reg_SG = prcomp(nba_off_reg_SG_vars,scale=TRUE)
pca_off_reg_SG
autoplot(pca_off_reg_SG, data = nba_off_reg_SG_vars, loadings = TRUE, loadings.label = TRUE)
#FGA PTS TwoP_Per FG_Per FT_Per ThreeP_Per

nba_off_adv_SG_vars = nba_off_adv_SG[,c(3:8)]
pca_off_adv_SG = prcomp(nba_off_adv_SG_vars,scale=TRUE)
pca_off_adv_SG
autoplot(pca_off_adv_SG, data = nba_off_adv_SG_vars, loadings = TRUE, loadings.label = TRUE,labels=Player)
#OWS FTr OBPM TS_Per

nba_def_SG_vars = nba_def_SG[,c(3:10)]
pca_def_SG = prcomp(nba_def_SG_vars,scale=TRUE)
pca_def_SG
autoplot(pca_def_SG, data = nba_def_SG_vars, loadings = TRUE, loadings.label = TRUE)
#DWS DRB DBPM STL

nba_ovr_SG_vars = nba_ovr_SG[,c(3:8)]
pca_ovr_SG = prcomp(nba_ovr_SG_vars,scale=TRUE)
pca_ovr_SG
autoplot(pca_ovr_SG, data = nba_ovr_SG_vars, loadings = TRUE, loadings.label = TRUE)
#PER USG_Per

##### Random Forest PCA Features SG #####

set.seed(1)
forest_SG=randomForest(SalaryUSD~PTS+TwoP_Per+FT_Per+ThreeP_Per+OWS+OBPM+TS_Per+DWS+DBPM+USG_Per, ntree=500, data=nba_SG,importance=TRUE)
forest_SG
#removed PER FG_Per colin
#drop STL DRB FTr from importance

importance(forest_SG)
predicted_salary_SG=predict(forest_SG,data=nba_SG)
predicted_salary_SG
Dif = nba_SG$SalaryUSD-predicted_salary_SG
Salary_SG = data.frame(nba_SG$Player,nba_SG$Tm,predicted_salary_SG,nba_SG$SalaryUSD,Dif)

#do.trace
forest_SG=randomForest(SalaryUSD~PTS+TwoP_Per+FT_Per+ThreeP_Per+OWS+OBPM+TS_Per+DWS+DBPM+USG_Per, ntree=500, do.trace=50,data=nba_SG,importance=TRUE)

##### PCA SF #####

nba_off_reg_SF_vars= nba_off_reg_SF[,c(3:18)]
pca_off_reg_SF = prcomp(nba_off_reg_SF_vars,scale=TRUE)
pca_off_reg_SF
autoplot(pca_off_reg_SF, data = nba_off_reg_SF_vars, loadings = TRUE, loadings.label = TRUE)
#FGA PTS TOV FT AST FTA TwoPA

nba_off_adv_SF_vars = nba_off_adv_SF[,c(3:8)]
pca_off_adv_SF = prcomp(nba_off_adv_SF_vars,scale=TRUE)
pca_off_adv_SF
autoplot(pca_off_adv_SF, data = nba_off_adv_SF_vars, loadings = TRUE, loadings.label = TRUE,labels=Player)
#OWS OBPM TS_Per

nba_def_SF_vars = nba_def_SF[,c(3:10)]
pca_def_SF = prcomp(nba_def_SF_vars,scale=TRUE)
pca_def_SF
autoplot(pca_def_SF, data = nba_def_SF_vars, loadings = TRUE, loadings.label = TRUE)
#DWS DRB

nba_ovr_SF_vars = nba_ovr_PF[,c(3:8)]
pca_ovr_SF = prcomp(nba_ovr_SF_vars,scale=TRUE)
pca_ovr_SF
autoplot(pca_ovr_SF, data = nba_ovr_SF_vars, loadings = TRUE, loadings.label = TRUE)
#VORP BPM WS

##### Random Forest PCA Features SF #####

set.seed(1)
forest_SF=randomForest(SalaryUSD~FGA+PTS+TOV+FT+AST+TwoPA+OWS+TS_Per+DRB+BPM, ntree=500, data=nba_SF,importance=TRUE)
forest_SF
#removed OBPM WS VORP for collinearity 

importance(forest_SF)
predicted_salary_SF=predict(forest_SF,data=nba_SF)
predicted_salary_SF
Dif = nba_SF$SalaryUSD-predicted_salary_SF
Salary_SF = data.frame(nba_SF$Player,nba_SF$Tm,predicted_salary_SF,nba_SF$SalaryUSD,Dif)

#do.trace
forest_SF=randomForest(SalaryUSD~FGA+PTS+TOV+FT+AST+TwoPA+OWS+TS_Per+DRB+BPM, ntree=500, do.trace=50,data=nba_SF,importance=TRUE)

##### PCA PF #####

nba_off_reg_PF_vars = nba_off_reg_PF[,c(3:18)]
pca_off_reg_PF = prcomp(nba_off_reg_PF_vars,scale=TRUE)
pca_off_reg_PF
autoplot(pca_off_reg_PF, data = nba_off_reg_PF_vars, loadings = TRUE, loadings.label = TRUE)
#ThreePA FGA FT_Per AST FT PTS FTA

nba_off_adv_PF_vars = nba_off_adv_PF[,c(3:8)]
pca_off_adv_PF = prcomp(nba_off_adv_PF_vars,scale=TRUE)
pca_off_adv_PF
autoplot(pca_off_adv_PF, data = nba_off_adv_PF_vars, loadings = TRUE, loadings.label = TRUE,labels=Player)
#OWS TS_Per OBPM

nba_def_PF_vars = nba_def_PF[,c(3:10)]
pca_def_PF = prcomp(nba_def_PF_vars,scale=TRUE)
pca_def_PF
autoplot(pca_def_PF, data = nba_def_PF_vars, loadings = TRUE, loadings.label = TRUE)
#DWS DBPM

nba_ovr_PF_vars = nba_ovr_PF[,c(3:8)]
pca_ovr_PF = prcomp(nba_ovr_PF_vars,scale=TRUE)
pca_ovr_PF
autoplot(pca_ovr_PF, data = nba_ovr_PF_vars, loadings = TRUE, loadings.label = TRUE)
#VORP WS BPM

##### Random Forest PCA Features PF #####

set.seed(1)
forest_PF=randomForest(SalaryUSD~PTS+FTA+AST+TS_Per+OBPM+DWS+DBPM+VORP, ntree=500, data=nba_PF,importance=TRUE)
forest_PF
#removed OWS WS BPM FT colin
#removed ThreePA FGA FT_Per importance

importance(forest_PF)
predicted_salary_PF=predict(forest_PF,data=nba_PF)
predicted_salary_PF
Dif = nba_PF$SalaryUSD-predicted_salary_PF
Salary_PF = data.frame(nba_PF$Player,nba_PF$Tm,predicted_salary_PF,nba_PF$SalaryUSD,Dif)

#do.trace
forest_PF=randomForest(SalaryUSD~PTS+FTA+AST+TS_Per+OBPM+DWS+DBPM+VORP, ntree=500, do.trace=50,data=nba_PF,importance=TRUE)

##### PCA C #####

nba_off_reg_C_vars = nba_off_reg_C[,c(3:18)]
pca_off_reg_C = prcomp(nba_off_reg_C_vars,scale=TRUE)
pca_off_reg_C
autoplot(pca_off_reg_C, data = nba_off_reg_C_vars, loadings = TRUE, loadings.label = TRUE)
#FT TwoP FTA TwoPA PTS TOV FGA FT_Per ORB AST

nba_off_adv_C_vars = nba_off_adv_C[,c(3:8)]
pca_off_adv_C = prcomp(nba_off_adv_C_vars,scale=TRUE)
pca_off_adv_C
autoplot(pca_off_adv_C, data = nba_off_adv_C_vars, loadings = TRUE, loadings.label = TRUE,labels=Player)
#OWS OBPM TS_Per 

nba_def_C_vars = nba_def_C[,c(3:10)]
pca_def_C = prcomp(nba_def_C_vars,scale=TRUE)
pca_def_C
autoplot(pca_def_C, data = nba_def_C_vars, loadings = TRUE, loadings.label = TRUE)
#TRB DWS DRB DBPM

nba_ovr_C_vars = nba_ovr_C[,c(3:8)]
pca_ovr_C = prcomp(nba_ovr_C_vars,scale=TRUE)
pca_ovr_C
autoplot(pca_ovr_C, data = nba_ovr_C_vars, loadings = TRUE, loadings.label = TRUE)
#PER USG_PER WS VORP

##### Random Forest PCA Features C #####

set.seed(1)
forest_C=randomForest(SalaryUSD~TwoPA+PTS+TOV+TS_Per+TRB+OBPM+DBPM+USG_Per+DWS+VORP, ntree=500, data=nba_C,importance=TRUE)
forest_C
#removed TwoP PER WS DRB OWS
#remove FT_Per FT AST ORB from importance

importance(forest_C)
predicted_salary_C=predict(forest_C,data=nba_C)
predicted_salary_C
Dif = nba_C$SalaryUSD-predicted_salary_C
Salary_C = data.frame(nba_C$Player,nba_C$Tm,predicted_salary_C,nba_C$SalaryUSD,Dif)

#do.trace
forest_C=randomForest(SalaryUSD~TwoPA+PTS+TOV+TS_Per+TRB+OBPM+DBPM+USG_Per+DWS+VORP, ntree=500, data=nba_C,do.trace=50,importance=TRUE)

###### Clusters

##### Centers #####
##### OWS & OBPM for Centers
View(nba_off_C)

nba_off_C1= nba_off_C[,c(22,23)]
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
nba_off_C1= nba_off_C[,c(14,22)]
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


kmeans.ani(nba_off_C1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_off_C1),km.5$cluster)
View(clusters)

##### BPM & VORP For Centers
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

##### WS & VORP for Centers***
View(nba_ovr_C)

nba_ovr_C1= nba_ovr_C[,c(6,8)]
rownames(nba_ovr_C1)=nba_ovr_C$Player
plot(nba_ovr_C1)

km.2=kmeans(nba_ovr_C1,2)
km.3=kmeans(nba_ovr_C1,3)
km.4=kmeans(nba_ovr_C1,4)
km.5=kmeans(nba_ovr_C1,5)
km.6=kmeans(nba_ovr_C1,6)
km.7=kmeans(nba_ovr_C1,7)
km.8=kmeans(nba_ovr_C1,8)

plot(nba_ovr_C1,col=(km.2$cluster))
plot(nba_ovr_C1,col=(km.3$cluster))
plot(nba_ovr_C1,col=(km.4$cluster))
plot(nba_ovr_C1,col=(km.5$cluster))
plot(nba_ovr_C1,col=(km.6$cluster))
plot(nba_ovr_C1,col=(km.7$cluster))
plot(nba_ovr_C1,col=(km.8$cluster))

kmeans.ani(nba_ovr_C1, centers = 8, pch = 1:8, col = 1:8,)
clusters=data.frame(rownames(nba_ovr_C1),km.8$cluster)
View(clusters)

## Karl-Anthony Towns, Nikola Jokić	, Nikola Vučević	, Rudy Gobert	are the top players that contributed to Winshares and they are the hardest to replace

## Defensive Analysis on Centers

##### DWS & STL for Centers***
nba_def_C1= nba_def_C[,c(6,9)]
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

kmeans.ani(nba_def_C1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_def_C1),km.5$cluster)
View(clusters)

## Andre Drummond, Brook Lopez, Hassan Whiteside, Marc Gasol, Myles Turner, Nikola Jokić, Nikola Vučević, Rudy Gobert are the top stealers and they have the highest defensive win shares (makes them the best in the stealing category)

##### DWS & BLK for Centers***
nba_def_C1= nba_def_C[,c(7,9)]
rownames(nba_def_C1)=nba_def_C$Player
plot(nba_def_C1)

km.2=kmeans(nba_def_C1,2)
km.3=kmeans(nba_def_C1,3)
km.4=kmeans(nba_def_C1,4)
km.5=kmeans(nba_def_C1,5)
km.6=kmeans(nba_def_C1,6)
km.7=kmeans(nba_def_C1,7)
km.8=kmeans(nba_def_C1,8)

plot(nba_def_C1,col=(km.2$cluster))
plot(nba_def_C1,col=(km.3$cluster))
plot(nba_def_C1,col=(km.4$cluster))
plot(nba_def_C1,col=(km.5$cluster))
plot(nba_def_C1,col=(km.6$cluster))
plot(nba_def_C1,col=(km.7$cluster))
plot(nba_def_C1,col=(km.8$cluster))

kmeans.ani(nba_def_C1, centers = 8, pch = 1:8, col = 1:8,)
clusters=data.frame(rownames(nba_def_C1),km.8$cluster)
View(clusters)

## The best blockers in the league are: Anthony Davis,Brook Lopez, Hassan Whiteside,JaVale McGee, Myles Turner, Nerlens Noel

##### VORP & USG_Per for Centers***
nba_ovr_C1= nba_ovr_C[,c(5,8)]
rownames(nba_ovr_C1)=nba_ovr_C$Player
plot(nba_ovr_C1)

km.2=kmeans(nba_ovr_C1,2)
km.3=kmeans(nba_ovr_C1,3)
km.4=kmeans(nba_ovr_C1,4)
km.5=kmeans(nba_ovr_C1,5)
km.6=kmeans(nba_ovr_C1,6)
km.7=kmeans(nba_ovr_C1,7)

plot(nba_ovr_C1,col=(km.2$cluster))
plot(nba_ovr_C1,col=(km.3$cluster))
plot(nba_ovr_C1,col=(km.4$cluster))
plot(nba_ovr_C1,col=(km.5$cluster))
plot(nba_ovr_C1,col=(km.6$cluster))
plot(nba_ovr_C1,col=(km.7$cluster))

kmeans.ani(nba_ovr_C1, centers = 7, pch = 1:7, col = 1:7,)
clusters=data.frame(rownames(nba_ovr_C1),km.7$cluster)
View(clusters)

## These centers had a vorp of at least 1 and above (relatively hard to replace them), which had a usage time of around 27 to 37 minutes per game.
# Players are: Al Horford, Bam Adebayo, Brook Lopez, Clint Capela, Dewayne Dedmon, Dwight Powell, Jarrett Allen, Larry Nance, Mason Plumlee, Rudy Gobert,Steven Adams, Willie Cauley-Stein

#### VORP & DWS***
View(nba_C)

nba_C1 = nba_C[,c(35,40)]
rownames(nba_C1)= nba_C$Player
plot(nba_C1)

km.2=kmeans(nba_C1,2)
km.3=kmeans(nba_C1,3)
km.4=kmeans(nba_C1,4)
km.5=kmeans(nba_C1,5)
km.6=kmeans(nba_C1,6)

plot(nba_C1,col=(km.2$cluster))
plot(nba_C1,col=(km.3$cluster))
plot(nba_C1,col=(km.4$cluster))
plot(nba_C1,col=(km.5$cluster))
plot(nba_C1,col=(km.6$cluster))

kmeans.ani(nba_C1, centers = 6, pch = 1:6, col = 1:6,)
clusters=data.frame(rownames(nba_C1),km.6$cluster)
View(clusters)

## These players are the hardest to replace, due to their superb defensive abilities (translated into high DWS)
# Players: Anthony Davis, Karl-Anthony Towns,Nikola Jokić, Nikola Vučević, Rudy Gobert

#### VORP & OWS***
View(nba_C)

nba_C1 = nba_C[,c(34,40)]
rownames(nba_C1)= nba_C$Player
plot(nba_C1)

km.2=kmeans(nba_C1,2)
km.3=kmeans(nba_C1,3)
km.4=kmeans(nba_C1,4)
km.5=kmeans(nba_C1,5)
km.6=kmeans(nba_C1,6)
km.7=kmeans(nba_C1,7)
km.8=kmeans(nba_C1,8)

plot(nba_C1,col=(km.2$cluster))
plot(nba_C1,col=(km.3$cluster))
plot(nba_C1,col=(km.4$cluster))
plot(nba_C1,col=(km.5$cluster))
plot(nba_C1,col=(km.6$cluster))
plot(nba_C1,col=(km.7$cluster))
plot(nba_C1,col=(km.8$cluster))

kmeans.ani(nba_C1, centers = 8, pch = 1:8, col = 1:8,)
clusters=data.frame(rownames(nba_C1),km.8$cluster)
View(clusters)

## These centers are the hardest to replace due to their strong offensive abilities (measured in high OWS)
# Players: Brook Lopez, Marc Gasol,Mason Plumlee, Myles Turner

#### VORP & WS*** 
View(nba_C)

nba_C1 = nba_C[,c(36,40)]
rownames(nba_C1)= nba_C$Player
plot(nba_C1)

km.2=kmeans(nba_C1,2)
km.3=kmeans(nba_C1,3)
km.4=kmeans(nba_C1,4)
km.5=kmeans(nba_C1,5)
km.6=kmeans(nba_C1,6)

plot(nba_C1,col=(km.2$cluster))
plot(nba_C1,col=(km.3$cluster))
plot(nba_C1,col=(km.4$cluster))
plot(nba_C1,col=(km.5$cluster))
plot(nba_C1,col=(km.6$cluster))

kmeans.ani(nba_C1, centers = 6, pch = 1:6, col = 1:6,)
clusters=data.frame(rownames(nba_C1),km.6$cluster)
View(clusters)

## These players were the hardest to replace, due to their significant contribution to the winning shares. Two of them are in the raptors roster. 
#Bam Adebayo
#Brook Lopez
#Deandre Ayton
#DeAndre Jordan
#Ed Davis
#Enes Kanter
#Hassan Whiteside
#Jakob Pöltl
#JaVale McGee
#Kevon Looney
#Larry Nance
#Marc Gasol -- raptors
#Mason Plumlee
#Mitchell Robinson
#Myles Turner
#Serge Ibaka -- raptors
#Thomas Bryant
#Willie Cauley-Stein

#### VORP & OBPM***
View(nba_C)

nba_C1 = nba_C[,c(37,40)]
rownames(nba_C1)= nba_C$Player
plot(nba_C1)

km.2=kmeans(nba_C1,2)
km.3=kmeans(nba_C1,3)
km.4=kmeans(nba_C1,4)
km.5=kmeans(nba_C1,5)
km.6=kmeans(nba_C1,6)

plot(nba_C1,col=(km.2$cluster))
plot(nba_C1,col=(km.3$cluster))
plot(nba_C1,col=(km.4$cluster))
plot(nba_C1,col=(km.5$cluster))
plot(nba_C1,col=(km.6$cluster))

kmeans.ani(nba_C1, centers = 6, pch = 1:6, col = 1:6,)
clusters=data.frame(rownames(nba_C1),km.6$cluster)
View(clusters)

## These players were the hardest to replace due to their high OBPM  
## Anthony Davis, Karl-Anthony Towns, Nikola Jokić, Nikola Vučević, Rudy Gobert

#### VORP & DBPM***
nba_C1 = nba_C[,c(47,49)]
rownames(nba_C1)= nba_C$Player
plot(nba_C1)

km.2=kmeans(nba_C1,2)
km.3=kmeans(nba_C1,3)
km.4=kmeans(nba_C1,4)
km.5=kmeans(nba_C1,5)
km.6=kmeans(nba_C1,6)
km.6=kmeans(nba_C1,7)

plot(nba_C1,col=(km.2$cluster))
plot(nba_C1,col=(km.3$cluster))
plot(nba_C1,col=(km.4$cluster))
plot(nba_C1,col=(km.5$cluster))
plot(nba_C1,col=(km.6$cluster))
plot(nba_C1,col=(km.7$cluster))

kmeans.ani(nba_C1, centers = 7, pch = 1:7, col = 1:7,)
clusters=data.frame(rownames(nba_C1),km.7$cluster)
View(clusters)

## The players with the highest VORP due to their OBPM are Anthony Davis, Karl-Anthony Towns, Nikola Jokić, Nikola Vučević, Rudy Gobert

#### VORP & SalaryUSD
View(nba_C)

nba_C1 = nba_C[,c(8,40)]
rownames(nba_C1)= nba_C$Player
plot(nba_C1)

km.2=kmeans(nba_C1,2)
km.3=kmeans(nba_C1,3)
km.4=kmeans(nba_C1,4)
km.5=kmeans(nba_C1,5)
km.6=kmeans(nba_C1,6)

plot(nba_C1,col=(km.2$cluster))
plot(nba_C1,col=(km.3$cluster))
plot(nba_C1,col=(km.4$cluster))
plot(nba_C1,col=(km.5$cluster))
plot(nba_C1,col=(km.6$cluster))

kmeans.ani(nba_C1, centers = 6, pch = 1:6, col = 1:6,)
clusters=data.frame(rownames(nba_C1),km.6$cluster)
View(clusters)
## We can see that salaries are not considered a major indicator to VORP. However, it is worth noting that players who had a VORP of more than 1.5 (roughly), were considered to be in the highest salary cluster. But the cluster didn't show that as salary USD increases, replacing a player gets hardr (higher VORP). 


###### PowerForwards ###### 

##### OWS & OBPM for PowerForwards 
View(nba_PF)

nba_off_C1= nba_off_PF[,c(22,23)] 
rownames(nba_off_C1)=nba_off_PF$Player 
plot(nba_off_C1)

km.2=kmeans(nba_off_C1,2)
km.3=kmeans(nba_off_C1,3)
km.4=kmeans(nba_off_C1,4)
km.5=kmeans(nba_off_C1,5)
km.6=kmeans(nba_off_C1,6)
km.7=kmeans(nba_off_C1,7)

plot(nba_off_C1,col=(km.2$cluster))
plot(nba_off_C1,col=(km.3$cluster))
plot(nba_off_C1,col=(km.4$cluster))
plot(nba_off_C1,col=(km.5$cluster))
plot(nba_off_C1,col=(km.6$cluster))
plot(nba_off_C1,col=(km.7$cluster))

kmeans.ani(nba_off_C1, centers = 7, pch = 1:7, col = 1:7,)
clusters=data.frame(rownames(nba_off_C1),km.7$cluster)
View(clusters)

## Players with the highest OWS and OBPM are: 
#Boban Marjanović	
#Cristiano Felício	
#Deandre Ayton
#Dewayne Dedmon	
#Dwight Powell
#Karl-Anthony 
#Kosta Koufos

##### OWS & FTr*** 
View(nba_PF)

nba_off_PF1= nba_off_PF[,c(23,21)]
rownames(nba_off_PF1)=nba_PF$Player
plot(nba_off_PF1)

km.2=kmeans(nba_off_PF1,2)
km.3=kmeans(nba_off_PF1,3)
km.4=kmeans(nba_off_PF1,4)
km.5=kmeans(nba_off_PF1,5)
plot(nba_off_PF1,col=(km.2$cluster))
plot(nba_off_PF1,col=(km.3$cluster))
plot(nba_off_PF1,col=(km.4$cluster))
plot(nba_off_PF1,col=(km.5$cluster))

kmeans.ani(nba_off_PF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_off_PF1),km.5$cluster)
View(clusters)


##### BPM & VORP For Power Forwards**
View(nba_PF)

nba_off_PF1= nba_PF[,c(39,40)]
rownames(nba_off_PF1)=nba_PF$Player
plot(nba_off_PF1)

km.2=kmeans(nba_off_PF1,2)
km.3=kmeans(nba_off_PF1,3)
km.4=kmeans(nba_off_PF1,4)
km.5=kmeans(nba_off_PF1,5)

plot(nba_off_PF1,col=(km.2$cluster))
plot(nba_off_PF1,col=(km.3$cluster))
plot(nba_off_PF1,col=(km.4$cluster))
plot(nba_off_PF1,col=(km.5$cluster))

kmeans.ani(nba_off_PF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_off_PF1),km.5$cluster)
View(clusters)

##### PER & VORP For Power Forwards***
View(nba_PF)

nba_off_PF1= nba_PF[,c(29,40)]
rownames(nba_off_PF1)=nba_PF$Player
plot(nba_off_PF1)

km.2=kmeans(nba_off_PF1,2)
km.3=kmeans(nba_off_PF1,3)
km.4=kmeans(nba_off_PF1,4)
km.5=kmeans(nba_off_PF1,5)
km.6=kmeans(nba_off_PF1,6)
km.7=kmeans(nba_off_PF1,7)
km.7=kmeans(nba_off_PF1,8)
km.8=kmeans(nba_off_PF1,8)

plot(nba_off_PF1,col=(km.2$cluster))
plot(nba_off_PF1,col=(km.3$cluster))
plot(nba_off_PF1,col=(km.4$cluster))
plot(nba_off_PF1,col=(km.5$cluster))
plot(nba_off_PF1,col=(km.6$cluster))
plot(nba_off_PF1,col=(km.7$cluster))
plot(nba_off_PF1,col=(km.8$cluster))

kmeans.ani(nba_off_PF1, centers = 8, pch = 1:8, col = 1:8,)
clusters=data.frame(rownames(nba_off_PF1),km.8$cluster)
View(clusters)

##### WS & VORP for PowerForwards***
View(nba_PF)

nba_off_PF1= nba_PF[,c(36,40)]
rownames(nba_off_PF1)=nba_PF$Player
plot(nba_off_PF1)

km.2=kmeans(nba_off_PF1,2)
km.3=kmeans(nba_off_PF1,3)
km.4=kmeans(nba_off_PF1,4)

plot(nba_off_PF1,col=(km.2$cluster))
plot(nba_off_PF1,col=(km.3$cluster))
plot(nba_off_PF1,col=(km.4$cluster))

kmeans.ani(nba_off_PF1, centers = 4, pch = 1:4, col = 1:4,)
clusters=data.frame(rownames(nba_off_PF1),km.4$cluster)
View(clusters)

##### DWS & STL for Centers ***
View(nba_PF)

nba_def_PF1= nba_def_PF[,c(6,9)]
rownames(nba_def_PF1)=nba_PF$Player
plot(nba_def_PF1)

km.2=kmeans(nba_def_PF1,2)
km.3=kmeans(nba_def_PF1,3)
km.4=kmeans(nba_def_PF1,4)
km.5=kmeans(nba_def_PF1,5)

plot(nba_def_PF1,col=(km.2$cluster))
plot(nba_def_PF1,col=(km.3$cluster))
plot(nba_def_PF1,col=(km.4$cluster))
plot(nba_def_PF1,col=(km.5$cluster))

kmeans.ani(nba_def_PF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_def_PF1),km.5$cluster)
View(clusters)

##### DWS & BLK for Power Forwards 
View(nba_PF)

nba_def_PF1= nba_PF[,c(25,35)]
rownames(nba_def_PF1)=nba_PF$Player
plot(nba_def_PF1)

km.2=kmeans(nba_def_PF1,2)
km.3=kmeans(nba_def_PF1,3)
km.4=kmeans(nba_def_PF1,4)
km.5=kmeans(nba_def_PF1,5)

plot(nba_def_PF1,col=(km.2$cluster))
plot(nba_def_PF1,col=(km.3$cluster))
plot(nba_def_PF1,col=(km.4$cluster))
plot(nba_def_PF1,col=(km.5$cluster))

kmeans.ani(nba_def_PF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_def_PF1),km.5$cluster)
View(clusters)

##### VORP & WS for PowerForwards***
View(nba_PF)

nba_ovr_PF1 = nba_PF[,c(36,40)]
rownames(nba_ovr_PF1)=nba_PF$Player
plot(nba_ovr_PF1)

km.2=kmeans(nba_ovr_PF1,2)
km.3=kmeans(nba_ovr_PF1,3)
km.4=kmeans(nba_ovr_PF1,4)
km.5=kmeans(nba_ovr_PF1,5)

plot(nba_ovr_PF1,col=(km.2$cluster))
plot(nba_ovr_PF1,col=(km.3$cluster))
plot(nba_ovr_PF1,col=(km.4$cluster))
plot(nba_ovr_PF1,col=(km.5$cluster))

kmeans.ani(nba_ovr_PF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_ovr_PF1),km.5$cluster)
View(clusters)

#### VORP & DWS***
View(nba_PF)

nba_PF1 = nba_PF[,c(35,40)]
rownames(nba_PF1)= nba_PF$Player
plot(nba_PF1)

km.2=kmeans(nba_PF1,2)
km.3=kmeans(nba_PF1,3)
km.4=kmeans(nba_PF1,4)
km.5=kmeans(nba_PF1,5)

plot(nba_PF1,col=(km.2$cluster))
plot(nba_PF1,col=(km.3$cluster))
plot(nba_PF1,col=(km.4$cluster))
plot(nba_PF1,col=(km.5$cluster))

kmeans.ani(nba_PF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_PF1),km.5$cluster)
View(clusters)

#### VORP & OWS***
View(nba_PF)

nba_PF1 = nba_PF[,c(34,40)]
rownames(nba_PF1)= nba_PF$Player
plot(nba_PF1)

km.2=kmeans(nba_PF1,2)
km.3=kmeans(nba_PF1,3)
km.4=kmeans(nba_PF1,4)


plot(nba_PF1,col=(km.2$cluster))
plot(nba_PF1,col=(km.3$cluster))
plot(nba_PF1,col=(km.4$cluster))

kmeans.ani(nba_PF1, centers = 4, pch = 1:4, col = 1:4,)
clusters=data.frame(rownames(nba_PF1),km.4$cluster)
View(clusters)

#### VORP & OBPM
View(nba_PF)

nba_PF1 = nba_PF[,c(37,40)]
rownames(nba_PF1)= nba_PF$Player
plot(nba_PF1)

km.2=kmeans(nba_PF1,2)
km.3=kmeans(nba_PF1,3)
km.4=kmeans(nba_PF1,4)
km.5=kmeans(nba_PF1,5)

plot(nba_PF1,col=(km.2$cluster))
plot(nba_PF1,col=(km.3$cluster))
plot(nba_PF1,col=(km.4$cluster))
plot(nba_PF1,col=(km.5$cluster))

kmeans.ani(nba_PF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_PF1),km.5$cluster)
View(clusters)

#### VORP & DBPM***
View(nba_PF)

nba_PF1 = nba_PF[,c(38,40)]
rownames(nba_PF1)= nba_PF$Player
plot(nba_PF1)

km.2=kmeans(nba_PF1,2)
km.3=kmeans(nba_PF1,3)
km.4=kmeans(nba_PF1,4)
km.5=kmeans(nba_PF1,5)

plot(nba_PF1,col=(km.2$cluster))
plot(nba_PF1,col=(km.3$cluster))
plot(nba_PF1,col=(km.4$cluster))
plot(nba_PF1,col=(km.5$cluster))

kmeans.ani(nba_PF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_PF1),km.5$cluster)
View(clusters)

#### VORP & BPM
View(nba_PF)

nba_PF1 = nba_PF[,c(39,40)]
rownames(nba_PF1)= nba_PF$Player
plot(nba_PF1)

km.2=kmeans(nba_PF1,2)
km.3=kmeans(nba_PF1,3)
km.4=kmeans(nba_PF1,4)
km.5=kmeans(nba_PF1,5)
km.6=kmeans(nba_PF1,6)

plot(nba_PF1,col=(km.2$cluster))
plot(nba_PF1,col=(km.3$cluster))
plot(nba_PF1,col=(km.4$cluster))
plot(nba_PF1,col=(km.5$cluster))
plot(nba_PF1,col=(km.6$cluster))

kmeans.ani(nba_PF1, centers = 6, pch = 1:6, col = 1:6,)
clusters=data.frame(rownames(nba_PF1),km.6$cluster)
View(clusters)

#### VORP & SalaryUSD 
View(nba_PF)

nba_PF1 = nba_PF[,c(8,40)]
rownames(nba_PF1)= nba_PF$Player
plot(nba_PF1)

km.2=kmeans(nba_PF1,2)
km.3=kmeans(nba_PF1,3)
km.4=kmeans(nba_PF1,4)
km.5=kmeans(nba_PF1,5)
km.6=kmeans(nba_PF1,6)

plot(nba_PF1,col=(km.2$cluster))
plot(nba_PF1,col=(km.3$cluster))
plot(nba_PF1,col=(km.4$cluster))
plot(nba_PF1,col=(km.5$cluster))
plot(nba_PF1,col=(km.6$cluster))

kmeans.ani(nba_PF1, centers = 6, pch = 1:6, col = 1:6,)
clusters=data.frame(rownames(nba_PF1),km.6$cluster)
View(clusters)

###### Shooting Guards ###### 

#### VORP & SalaryUSD
View(nba_SG)

nba_SG1 = nba_SG[,c(8,40)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)
km.6=kmeans(nba_SG1,6)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))
plot(nba_SG1,col=(km.6$cluster))

kmeans.ani(nba_SG1, centers = 6, pch = 1:6, col = 1:6,)
clusters=data.frame(rownames(nba_SG1),km.6$cluster)
View(clusters)

#### SalaryUSD & ThreeP_Per
View(nba_SG)

nba_SG1 = nba_SG[,c(8,13)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))

kmeans.ani(nba_SG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SG1),km.5$cluster)
View(clusters)

#### SalaryUSD & FG_Per *
View(nba_SG)

nba_SG1 = nba_SG[,c(8,10)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))

kmeans.ani(nba_SG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SG1),km.5$cluster)
View(clusters)

#### VORP & FG_Per ***
View(nba_SG)

nba_SG1 = nba_SG[,c(10,40)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))

kmeans.ani(nba_SG1, centers = 5, pch = 1:5, col = 1:5,)
clusters1=data.frame(rownames(nba_SG1),km.5$cluster)
View(clusters1)

#### ThreeP_Per & FG_Per *** 
View(nba_SG)

nba_SG1 = nba_SG[,c(10,13)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))

kmeans.ani(nba_SG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SG1),km.5$cluster)
View(clusters)

#### OWS & WS ***
View(nba_SG)

nba_SG1 = nba_SG[,c(34,36)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))

kmeans.ani(nba_SG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SG1),km.5$cluster)
View(clusters)

#### AST & WS **
View(nba_SG)

nba_SG1 = nba_SG[,c(23,36)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))

kmeans.ani(nba_SG1, centers = 5, pch = 1:5, col = 1:5,)
clusters4=data.frame(rownames(nba_SG1),km.5$cluster)
View(clusters4)

#### Salary & OWS *
View(nba_SG)

nba_SG1 = nba_SG[,c(8,34)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))

kmeans.ani(nba_SG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SG1),km.5$cluster)
View(clusters)

#### OWS & VORP***
View(nba_SG)

nba_SG1 = nba_SG[,c(34,40)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))

kmeans.ani(nba_SG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SG1),km.5$cluster)
View(clusters)

#### VORP & Player Usage *** 
View(nba_SG)

nba_SG1 = nba_SG[,c(33,40)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))

kmeans.ani(nba_SG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SG1),km.5$cluster)
View(clusters)

#### WS & OBPM ***
View(nba_SG)

nba_SG1 = nba_SG[,c(36,37)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))

kmeans.ani(nba_SG1, centers = 5, pch = 1:5, col = 1:5,)
clusters2=data.frame(rownames(nba_SG1),km.5$cluster)
View(clusters2)

#### True shooting percentage and vorp ***
View(nba_SG)

nba_SG1 = nba_SG[,c(40,30)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))

kmeans.ani(nba_SG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SG1),km.5$cluster)
View(clusters)

#### ThreePAr and salary 
View(nba_SG)

nba_SG1 = nba_SG[,c(31,8)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))

kmeans.ani(nba_SG1, centers = 4, pch = 1:4, col = 1:4,)
clusters=data.frame(rownames(nba_SG1),km.4$cluster)
View(clusters)

#### DWS and VORP *** 
View(nba_SG)

nba_SG1 = nba_SG[,c(35,40)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))

kmeans.ani(nba_SG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SG1),km.5$cluster)
View(clusters)

#### ows and dws ***
View(nba_SG)

nba_SG1 = nba_SG[,c(34,35)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))

kmeans.ani(nba_SG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SG1),km.5$cluster)
View(clusters)

###### Small Forward ###### 

#### VORP & SalaryUSD 
View(nba_SF)

nba_SF1 = nba_SF[,c(8,40)]
rownames(nba_SF1)= nba_SF$Player
plot(nba_SF1)

km.2=kmeans(nba_SF1,2)
km.3=kmeans(nba_SF1,3)
km.4=kmeans(nba_SF1,4)
km.5=kmeans(nba_SF1,5)
km.6=kmeans(nba_SF1,6)

plot(nba_SF1,col=(km.2$cluster))
plot(nba_SF1,col=(km.3$cluster))
plot(nba_SF1,col=(km.4$cluster))
plot(nba_SF1,col=(km.5$cluster))
plot(nba_SF1,col=(km.6$cluster))

kmeans.ani(nba_SF1, centers = 6, pch = 1:6, col = 1:6,)
clusters=data.frame(rownames(nba_SF1),km.6$cluster)
View(clusters)

#### SalaryUSD & ThreeP *** 
View(nba_SF)

nba_SF1 = nba_SF[,c(8,11)]
rownames(nba_SF1)= nba_SF$Player
plot(nba_SF1)

km.2=kmeans(nba_SF1,2)
km.3=kmeans(nba_SF1,3)
km.4=kmeans(nba_SF1,4)
km.5=kmeans(nba_SF1,5)

plot(nba_SF1,col=(km.2$cluster))
plot(nba_SF1,col=(km.3$cluster))
plot(nba_SF1,col=(km.4$cluster))
plot(nba_SF1,col=(km.5$cluster))

kmeans.ani(nba_SF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SF1),km.5$cluster)
View(clusters)

#### OWS & DWS *** 
View(nba_SF)

nba_SF1 = nba_SF[,c(11,40)]
rownames(nba_SG1)= nba_SG$Player
plot(nba_SG1)

km.2=kmeans(nba_SG1,2)
km.3=kmeans(nba_SG1,3)
km.4=kmeans(nba_SG1,4)
km.5=kmeans(nba_SG1,5)

plot(nba_SG1,col=(km.2$cluster))
plot(nba_SG1,col=(km.3$cluster))
plot(nba_SG1,col=(km.4$cluster))
plot(nba_SG1,col=(km.5$cluster))

kmeans.ani(nba_SG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SG1),km.5$cluster)
View(clusters)

#### ThreeP_Per & FG_Per ** 
View(nba_SF)

nba_SF1 = nba_SF[,c(10,13)]
rownames(nba_SF1)= nba_SF$Player
plot(nba_SF1)

km.2=kmeans(nba_SF1,2)
km.3=kmeans(nba_SF1,3)
km.4=kmeans(nba_SF1,4)
km.5=kmeans(nba_SF1,5)

plot(nba_SF1,col=(km.2$cluster))
plot(nba_SF1,col=(km.3$cluster))
plot(nba_SF1,col=(km.4$cluster))
plot(nba_SF1,col=(km.5$cluster))

kmeans.ani(nba_SF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SF1),km.5$cluster)
View(clusters)

#### OWS & WS
View(nba_SF)

nba_SF1 = nba_SF[,c(36,34)]
rownames(nba_SF1)= nba_SF$Player
plot(nba_SF1)

km.2=kmeans(nba_SF1,2)
km.3=kmeans(nba_SF1,3)
km.4=kmeans(nba_SF1,4)
km.5=kmeans(nba_SF1,5)

plot(nba_SF1,col=(km.2$cluster))
plot(nba_SF1,col=(km.3$cluster))
plot(nba_SF1,col=(km.4$cluster))
plot(nba_SF1,col=(km.5$cluster))

kmeans.ani(nba_SF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SF1),km.5$cluster)
View(clusters)

#### AST & WS *** 
View(nba_SF)

nba_SF1 = nba_SF[,c(23,36)]
rownames(nba_SF1)= nba_SF$Player
plot(nba_SF1)

km.2=kmeans(nba_SF1,2)
km.3=kmeans(nba_SF1,3)
km.4=kmeans(nba_SF1,4)
km.5=kmeans(nba_SF1,5)

plot(nba_SF1,col=(km.2$cluster))
plot(nba_SF1,col=(km.3$cluster))
plot(nba_SF1,col=(km.4$cluster))
plot(nba_SF1,col=(km.5$cluster))

kmeans.ani(nba_SF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SF1),km.5$cluster)
View(clusters)

#### Salary & OWS
View(nba_SF)

nba_SF1 = nba_SF[,c(8,34)]
rownames(nba_SF1)= nba_SF$Player
plot(nba_SF1)

km.2=kmeans(nba_SF1,2)
km.3=kmeans(nba_SF1)
km.4=kmeans(nba_SF1,4)

plot(nba_SF1,col=(km.2$cluster))
plot(nba_SF1,col=(km.3$cluster))
plot(nba_SF1,col=(km.4$cluster))

kmeans.ani(nba_SF1, centers = 4, pch = 1:4, col = 1:4,)
clusters=data.frame(rownames(nba_SF1),km.4$cluster)
View(clusters)

#### OWS & VORP ***
View(nba_SF)

nba_SF1 = nba_SF[,c(34,40)]
rownames(nba_SF1)= nba_SF$Player
plot(nba_SF1)

km.2=kmeans(nba_SF1,2)
km.3=kmeans(nba_SF1,3)
km.4=kmeans(nba_SF1,4)
km.5=kmeans(nba_SF1,5)

plot(nba_SF1,col=(km.2$cluster))
plot(nba_SF1,col=(km.3$cluster))
plot(nba_SF1,col=(km.4$cluster))
plot(nba_SF1,col=(km.5$cluster))

kmeans.ani(nba_SF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SF1),km.5$cluster)
View(clusters)

#### VORP & Player Usage *** 
View(nba_SF)

nba_SF1 = nba_SF[,c(33,40)]
rownames(nba_SF1)= nba_SF$Player
plot(nba_SF1)

km.2=kmeans(nba_SF1,2)
km.3=kmeans(nba_SF1,3)
km.4=kmeans(nba_SF1,4)
km.5=kmeans(nba_SF1,5)

plot(nba_SF1,col=(km.2$cluster))
plot(nba_SF1,col=(km.3$cluster))
plot(nba_SF1,col=(km.4$cluster))
plot(nba_SF1,col=(km.5$cluster))

kmeans.ani(nba_SF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SF1),km.5$cluster)
View(clusters)


#### True shooting percentage and vorp 
View(nba_SF)

nba_SF1 = nba_SF[,c(30,40)]
rownames(nba_SF1)= nba_SF$Player
plot(nba_SF1)

km.2=kmeans(nba_SF1,2)
km.3=kmeans(nba_SF1,3)
km.4=kmeans(nba_SF1,4)
km.5=kmeans(nba_SF1,5)

plot(nba_SF1,col=(km.2$cluster))
plot(nba_SF1,col=(km.3$cluster))
plot(nba_SF1,col=(km.4$cluster))
plot(nba_SF1,col=(km.5$cluster))

kmeans.ani(nba_SF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SF1),km.5$cluster)
View(clusters)

#### ThreePAr and salary 
View(nba_SF)

nba_SF1 = nba_SF[,c(8,31)]
rownames(nba_SF1)= nba_SF$Player
plot(nba_SF1)

km.2=kmeans(nba_SF1,2)
km.3=kmeans(nba_SF1,3)
km.4=kmeans(nba_SF1,4)
km.5=kmeans(nba_SF1,5)

plot(nba_SF1,col=(km.2$cluster))
plot(nba_SF1,col=(km.3$cluster))
plot(nba_SF1,col=(km.4$cluster))
plot(nba_SF1,col=(km.5$cluster))

kmeans.ani(nba_SF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SF1),km.5$cluster)
View(clusters)

#### dws and vorp *** 
View(nba_SF)

nba_SF1 = nba_SF[,c(35,40)]
rownames(nba_SF1)= nba_SF$Player
plot(nba_SF1)

km.2=kmeans(nba_SF1,2)
km.3=kmeans(nba_SF1,3)
km.4=kmeans(nba_SF1,4)
km.5=kmeans(nba_SF1,5)

plot(nba_SF1,col=(km.2$cluster))
plot(nba_SF1,col=(km.3$cluster))
plot(nba_SF1,col=(km.4$cluster))
plot(nba_SF1,col=(km.5$cluster))

kmeans.ani(nba_SF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SF1),km.5$cluster)
View(clusters)

#### ows and dws *** 
View(nba_SF)

nba_SF1 = nba_SF[,c(34,35)]
rownames(nba_SF1)= nba_SF$Player
plot(nba_SF1)

km.2=kmeans(nba_SF1,2)
km.3=kmeans(nba_SF1,3)
km.4=kmeans(nba_SF1,4)
km.5=kmeans(nba_SF1,5)

plot(nba_SF1,col=(km.2$cluster))
plot(nba_SF1,col=(km.3$cluster))
plot(nba_SF1,col=(km.4$cluster))
plot(nba_SF1,col=(km.5$cluster))

kmeans.ani(nba_SF1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_SF1),km.5$cluster)
View(clusters)

###### Point guards: Clusters ###### 

## ows and dws *** 
View(nba_PG)

nba_PG1 = nba_PG[,c(34,35)]
rownames(nba_PG1)= nba_PG$Player
plot(nba_PG1)

km.2=kmeans(nba_PG1,2)
km.3=kmeans(nba_PG1,3)
km.4=kmeans(nba_PG1,4)
km.5=kmeans(nba_PG1,5)

plot(nba_PG1,col=(km.2$cluster))
plot(nba_PG1,col=(km.3$cluster))
plot(nba_PG1,col=(km.4$cluster))
plot(nba_PG1,col=(km.5$cluster))

kmeans.ani(nba_PG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_PG1),km.5$cluster)
View(clusters)

## vorp and ast *** 
View(nba_PG)

nba_PG1 = nba_PG[,c(23,40)]
rownames(nba_PG1)= nba_PG$Player
plot(nba_PG1)

km.2=kmeans(nba_PG1,2)
km.3=kmeans(nba_PG1,3)
km.4=kmeans(nba_PG1,4)
km.5=kmeans(nba_PG1,5)

plot(nba_PG1,col=(km.2$cluster))
plot(nba_PG1,col=(km.3$cluster))
plot(nba_PG1,col=(km.4$cluster))
plot(nba_PG1,col=(km.5$cluster))

kmeans.ani(nba_PG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_PG1),km.5$cluster)
View(clusters)

## vorp and threePAr
View(nba_PG)

nba_PG1 = nba_PG[,c(31,40)]
rownames(nba_PG1)= nba_PG$Player
plot(nba_PG1)

km.2=kmeans(nba_PG1,2)
km.3=kmeans(nba_PG1,3)
km.4=kmeans(nba_PG1,4)
km.5=kmeans(nba_PG1,5)

plot(nba_PG1,col=(km.2$cluster))
plot(nba_PG1,col=(km.3$cluster))
plot(nba_PG1,col=(km.4$cluster))
plot(nba_PG1,col=(km.5$cluster))

kmeans.ani(nba_PG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_PG1),km.5$cluster)
View(clusters)

## vorp and usage percentage *** 
View(nba_PG)

nba_PG1 = nba_PG[,c(33,40)]
rownames(nba_PG1)= nba_PG$Player
plot(nba_PG1)

km.2=kmeans(nba_PG1,2)
km.3=kmeans(nba_PG1,3)
km.4=kmeans(nba_PG1,4)
km.5=kmeans(nba_PG1,5)

plot(nba_PG1,col=(km.2$cluster))
plot(nba_PG1,col=(km.3$cluster))
plot(nba_PG1,col=(km.4$cluster))
plot(nba_PG1,col=(km.5$cluster))

kmeans.ani(nba_PG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_PG1),km.5$cluster)
View(clusters)

## vorp and usd salary *** 
View(nba_PG)

nba_PG1 = nba_PG[,c(8,40)]
rownames(nba_PG1)= nba_PG$Player
plot(nba_PG1)

km.2=kmeans(nba_PG1,2)
km.3=kmeans(nba_PG1,3)
km.4=kmeans(nba_PG1,4)
km.5=kmeans(nba_PG1,5)

plot(nba_PG1,col=(km.2$cluster))
plot(nba_PG1,col=(km.3$cluster))
plot(nba_PG1,col=(km.4$cluster))
plot(nba_PG1,col=(km.5$cluster))

kmeans.ani(nba_PG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_PG1),km.5$cluster)
View(clusters)

## threePAr and usd salary
View(nba_PG)

nba_PG1 = nba_PG[,c(8,31)]
rownames(nba_PG1)= nba_PG$Player
plot(nba_PG1)

km.2=kmeans(nba_PG1,2)
km.3=kmeans(nba_PG1,3)
km.4=kmeans(nba_PG1,4)
km.5=kmeans(nba_PG1,5)

plot(nba_PG1,col=(km.2$cluster))
plot(nba_PG1,col=(km.3$cluster))
plot(nba_PG1,col=(km.4$cluster))
plot(nba_PG1,col=(km.5$cluster))

kmeans.ani(nba_PG1, centers = 5, pch = 1:5, col = 1:5,)
clusters=data.frame(rownames(nba_PG1),km.5$cluster)
View(clusters)

## ows and true shooting percentage *** 
View(nba_PG)

nba_PG1 = nba_PG[,c(34,30)]
rownames(nba_PG1)= nba_PG$Player
plot(nba_PG1)

km.2=kmeans(nba_PG1,2)
km.3=kmeans(nba_PG1,3)
km.4=kmeans(nba_PG1,4)
km.5=kmeans(nba_PG1,5)

plot(nba_PG1,col=(km.2$cluster))
plot(nba_PG1,col=(km.3$cluster))
plot(nba_PG1,col=(km.4$cluster))
plot(nba_PG1,col=(km.5$cluster))

kmeans.ani(nba_PG1, centers = 5, pch = 1:5, col = 1:5,)
clusters2=data.frame(rownames(nba_PG1),km.5$cluster)
View(clusters2)

## ows and bpm ***
View(nba_PG)

nba_PG1 = nba_PG[,c(34,39)]
rownames(nba_PG1)= nba_PG$Player
plot(nba_PG1)

km.2=kmeans(nba_PG1,2)
km.3=kmeans(nba_PG1,3)
km.4=kmeans(nba_PG1,4)
km.5=kmeans(nba_PG1,5)
km.6=kmeans(nba_PG1,6)

plot(nba_PG1,col=(km.2$cluster))
plot(nba_PG1,col=(km.3$cluster))
plot(nba_PG1,col=(km.4$cluster))
plot(nba_PG1,col=(km.5$cluster))
plot(nba_PG1,col=(km.6$cluster))

kmeans.ani(nba_PG1, centers = 6, pch = 1:6, col = 1:6,)
clusters3=data.frame(rownames(nba_PG1),km.6$cluster)
View(clusters3)






###### Visualization ###### 

### Graph #1: Overpaid and underpaid NBA Teams 
# We can add different colors here 
Overpayment = c(-2572398.54358824, -2374778.03571429, -2265030.18583333, -2055800.8181875, -1492787.86383333, -1260929.26194118, -1250388.654, -1088120.93116667, -1084366.02564286, -878441.193642857, -708888.029357143, -615113.905647059, -490975.817722222, -464325.733210526, -318697.136789474, -61947.1685714285, -55908.2896000003, 69082.4103571427, 140054.909411764,179498.592133333, 315815.733533333, 359754.1048,852755.334066667, 930045.356421053,992700.545571429, 1199400.45194444, 1525201.93689474, 1648521.80364286,2216500.325125,2277719.21530769)      
print(Overpayment) 
Teams = c("LAL","BRK","PHI","SAC","ATL","LAC","ORL","PHO","IND","DAL","MIL","GSW","NYK",
          "CHO",
          "TOR",
          "HOU",
          "POR",
          "UTA",
          "NOP",
          "SAS",
          "DEN",
          "BOS",
          "DET",
          "MEM",
          "MIA",
          "MIN",
          "CLE",
          "CHI",
          "WAS",
          "OKC")

data<- data.frame("Team" = Teams, "Total Overpaid/Underpaid budget per team" = Budget)
print(data)

ggplot(data, aes(x=Teams, y=Overpayment)) + geom_point(aes(size=Overpayment)) + labs(x = "NBA Teams", y = "Total overpaid or underpaid amount per team") + ggtitle("Overpaid and underpaid amount across NBA teams")

#### Graph #2 (select three): 
## FT%(X) with respect to Salary(Y) -- grouped by players that made ft_percentage above 70%
# Ellipse by groups
a <- ggplot(nba, aes(FT_Per, SalaryUSD, color = FT_Per > 0.7))+
  geom_point()
a + stat_ellipse()
# Change the type of ellipses: possible values are "t", "norm", "euclid"
a + stat_ellipse(type = "norm") + labs(x = "Value of Replacing Player (VORP)", y = "Free Throw Percentage") + ggtitle("Freethrow percentage vs Value of Replacing Players with Free Throw Percentage above 85%")

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
c + stat_ellipse(type = "norm") + labs(x = "Value of Replacing Player (VORP)", y = "Salaries (in USD)") + ggtitle("Value of Replacing Players with a VORP of more than 1 (relatively hard to replace) as a function of players salaries ")

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
e + stat_ellipse(type = "norm") + labs(x = "Value of Replacing Player (VORP)", y = "Free Throw Percentage") + ggtitle("Value of Replacing Players with Free Throw Percentage more than 85% per 100 posessions")

#### Graph #3 (select four): 

## Age(X) with respect to Salary(Y)
ggplot(data = nba, aes(x = nba$Age, y = nba$SalaryUSD)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple") + labs (x = "Age of NBA players (in years)", y = "Salaries of NBA Players (in USD)") + ggtitle("Analzing NBA Salaries with respect to players' age")

## SalaryUSD(X) with respect to VORP(Y)
ggplot(data = nba, aes(x = SalaryUSD, y = VORP)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple", formula = y~poly(x,3)) + labs (x = " Salaries of NBA Players (in USD)", y = " Value of Replacing Players (VORP)") + ggtitle("Analyzing ")

## OWS(X) with respect to VORP(Y)
fig_ows <- ggplot(data = nba, aes(x = nba$OWS, y = VORP)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple")

## DWS(X) with respect to VORP(Y)
fig_dws <- ggplot(data = nba, aes(x = nba$DWS, y = VORP)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple")

## Minutes Played(X) with respect to VORP(Y)
mins_vorp <- ggplot(data = nba, aes(x = nba$MP, y = VORP)) +
  geom_point() + geom_smooth(method='lm', color = "red", fill = "purple")


#### combining the above graph
install.packages("magrittr")
library(magrittr)

install.packages("ggpubr")
library(ggplot2)
library(ggpubr)

ggarrange(fig_ows,fig_dws,  labels = c("Offensive Win Shares with respect to the Value of Replacing a Player (VORP)", "Defensive Win Shares with respect to the Value of Replacing a Player (VORP)"))


