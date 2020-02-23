rm(list = ls())

library(dplyr)
library(animation)
library(randomForest)
library(ggplot2) 
library(ggfortify)
library(gbm)

attach(nba)
nba = nba%>%filter(MP>49)
nba = na.omit(nba)
glimpse(nba)
summary(nba)
str(nba)

nba_labels=nba[,c(1:8)]
nba_off_reg=nba[,c(1,2,8,9:20,23,26,28)]
nba_off_adv=nba[,c(1,2,8,30,31,32,34,37)]
nba_def=nba[,c(1,2,8,21,22,24,25,27,35,38)]
nba_ovr=nba[,c(1,2,8,29,33,36,39,40)]

nba_C = nba%>%filter(Pos=='C')
nba_off_reg_C = nba_off_reg%>%filter(Pos=='C')
nba_off_adv_C = nba_off_adv%>%filter(Pos=='C')
nba_def_C = nba_def%>%filter(Pos=='C')
nba_ovr_C = nba_ovr%>%filter(Pos=='C')
nba_PF = nba%>%filter(Pos=='PF')
nba_off_reg_PF = nba_off_reg%>%filter(Pos=='PF')
nba_off_adv_PF = nba_off_adv%>%filter(Pos=='PF')
nba_def_PF = nba_def%>%filter(Pos=='PF')
nba_ovr_PF = nba_ovr%>%filter(Pos=='PF')
nba_SF = nba%>%filter(Pos=='SF')
nba_off_reg_SF = nba_off_reg%>%filter(Pos=='SF')
nba_off_adv_SF = nba_off_adv%>%filter(Pos=='SF')
nba_def_SF = nba_def%>%filter(Pos=='SF')
nba_ovr_SF = nba_ovr%>%filter(Pos=='SF')
nba_SG = nba%>%filter(Pos=='SG')
nba_off_reg_SG = nba_off_reg%>%filter(Pos=='SG')
nba_off_adv_SG = nba_off_adv%>%filter(Pos=='SG')
nba_def_SG = nba_def%>%filter(Pos=='SG')
nba_ovr_SG = nba_ovr%>%filter(Pos=='SG')
nba_PG = nba%>%filter(Pos=='PG')
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








