################################Reading data from csv file #################

setwd('C:\\Users\\sogunda\\Desktop\\DataScience\\INSOFE\\CUTe2')
crimedata_original<-read.csv('crimedata.csv',header=T)

#############Problem Statement : ##########
##Based on the attributes given in the data,we need to build a model to predict the (ViolentCrimesPerPopop)

#############Hypothesis: ############
#H0(Null Hypothesis):ViolentCrimesPerPopop (Target Variable) doesn't depend on the attributes in the data and the (beta)co-efficients of the attributes are considered to be 0
#H1(Alternate Hypothesis):ViolentCrimesPerPopop (Target Variable) depends on the attributes in the data and the (beta)co-efficients of the attributes are considered to be non-zero

#################################Data Exploration ##########################

str(crimedata_original)
summary(crimedata_original)
sum(is.na(crimedata_original))
rowSums(is.na(crimedata_original))
colSums(is.na(crimedata_original))

###################################Data Preprocessing ######################

library(dplyr)
missingcols<- colnames(crimedata_original[,colSums(is.na(crimedata_original))>100])
print(missingcols)
##Datset-After removing Max Missing NA columns 
crimedata<- crimedata_original[ , !(names(crimedata_original) %in% missingcols)] 
##Max columns having  NA columns
missingNAData<- crimedata_original[ , (names(crimedata_original) %in% missingcols)]

sum(is.na(crimedata))
##A row having 1 NA value in entire dataset is removed instead of imputation as it cost computation
crimedata<- crimedata[-131,] 
##Removing county and Community as they are not predicted values
crimedata<- subset(crimedata,select=-c(1:2))

#####Train-test split of data #######
library(caret)
set.seed(123)
trainrows <- createDataPartition(crimedata$ViolentCrimesPerPop,p = 0.7, list = F)
train <- crimedata[trainrows,]
validation <- crimedata[-trainrows,]
dim(train)
dim(validation)

################Common Function to calcualte the metrics of the models #################

ModelBuilding<- function(lm_model,lm_summary,train,validation,text){
  result_colnames<- c('train_mse','train_rmse','validation_mse','validation_rmse')
  
  
  #Residual = Observed value - fitted value (e = y - y^)
  residuals <- lm_summary$residuals
  hist(lm_model$residuals,main = paste0('Histogram of Residual vs Frequency of ',
                                        text,'model'))
  
  R2Squared   <-lm_summary$r.squared
  AdjR2Squared<-lm_summary$adj.r.squared
  f_stats <- lm_summary$fstatistic 
  model_p <- pf(f_stats[1], f_stats[2], f_stats[3], lower=FALSE)
  
  # CIs for model parameters 
  #confint(lm_model, level=0.95)
  
  # anova table
  lm_modelanova<- anova(lm_model)

  ##predicted values by the model when we created te linear model 
  fittedvalues<- lm_model$fitted.values
  
  ##Leverage values 
  leveragevalues<- hatvalues(model1)

  ##Cook's Distance 
  cooksdistance<-cooks.distance(lm_model)
  
  # # regression diagnostics
  # influencedetails<- influence.measures(lm_model)
  # summary(influencedetails)
  
  # For model comparison, the model with the lowest AIC and BIC score is preferred.
  AIC<-AIC(lm_model) #The Akaike's information criterion 
  BIC<-BIC(lm_model) #The Bayesian information criterion 
  
  par(mfrow=c(2,2)) 
  plot(lm_model)
  
  require(DMwR)
  options("scipen"=100)
  
  ##To evaluate the errors of the linear model on train data
  trainpredvalues<- predict(lm_model,newdata = train)
  
  moderrors<-data.frame(regr.eval(train$ViolentCrimesPerPop,trainpredvalues))
  moderrors<-as.data.frame(t(moderrors))

  ##To evaluate the errors of the linear model on validation data
  validationpredvalues<- predict(lm_model,newdata = validation)
  moderrors2<-data.frame(regr.eval(validation$ViolentCrimesPerPop,
                                   validationpredvalues))
  moderrors2<-as.data.frame(t(moderrors2))
  print(moderrors2)
  ##Calculation of prediction accuracy and error rates####
  actuals_preds <- data.frame(cbind(actuals=validation$ViolentCrimesPerPop, 
                                    predicteds=validationpredvalues))  #actuals_predicteds dataframe.
  correlation_accuracy <- data.frame(cor(actuals_preds)) 
  
  result<-cbind(moderrors[2:3],moderrors2[2:3])
  names(result)<-result_colnames
  return(result)
}


################## Model1- Modelling on entire Dataset##################################

model1<- lm(data = train,formula = ViolentCrimesPerPop~.)
m1summary<- summary(model1)
modelmetrics<-ModelBuilding(model1,m1summary,train,validation,'Model1(Entire data)')
print(modelmetrics)

################## Model2- Modelling after removing leverage and outliers###############
##Checking for leverage points and cook's distance ,so we can remove the outliers/or the values which are deviating more from the data.

#Leverage points
par(mfrow=c(1,1))
lev<- hat(model.matrix(model1))
plot(lev,main='Leverage Plot of Model1')
levpoints<-train[lev>0.25,]

#Cook's distance points
cooks<- cooks.distance(model1)
par(mfrow=c(1,1))
plot(cooks,main='Cook\'s distance Plot of Model1')
max<-as.numeric(which(cooks > 0.020))
points(max,cooks[max],col='red',pch=19)

#Considering the lev limit to >0.25 and cook's distance max point, we are retrieving the values and deleting them from dataset
row.names(train[which(lev>0.25),])
points_to_delete<-c(as.numeric(which(lev>0.25)),as.numeric(which(cooks > 0.020)))
train[points_to_delete,] ##Original train data
train2<-train[-points_to_delete,] ##Train data after removing leverage and cook's distance points thresold values

model2<- lm(data = train2,formula = ViolentCrimesPerPop~.)
m2summary<- summary(model2)
modelmetrics2<-ModelBuilding(model2,m2summary,train2,validation,'Model2')
print(modelmetrics2)

modelmetrics<-rbind(modelmetrics,modelmetrics2)

require(MASS)
shapiro<-shapiro.test(model2$residuals)
print(shapiro)
shapiro$p.value

hist(train2$ViolentCrimesPerPop)
hist(model2$residuals)

############################################################################

train_target<- train2$ViolentCrimesPerPop
validation_target<-validation$ViolentCrimesPerPop

traindata<-train2[-101]
validationdata<-validation[-101]

###########################Model3-Modelling using StepAIC-Both#############################
library(MASS)
step <- stepAIC(model2, direction="both")
summary(step)

par(mfrow=c(2,2))
plot(step)

#Look at the residuals to see if there are any patterns
par(mfrow=c(1,1))
plot(step$fitted.values,step$residuals)

##Model2-O/p of the the step-AIC linear equation####
lm_step_model<-lm( ViolentCrimesPerPop ~ racepctblack + agePct12t29 + 
                     numbUrban + pctUrban + medIncome + pctWWage + pctWFarmSelf + 
                     pctWInvInc + pctWSocSec + pctWRetire + medFamInc + whitePerCap + 
                     indianPerCap + AsianPerCap + OtherPerCap + PctPopUnderPov + 
                     PctLess9thGrade + PctEmploy + PctEmplManu + PctOccupManu + 
                     PctOccupMgmtProf + MalePctDivorce + MalePctNevMarr + FemalePctDiv + 
                     TotalPctDiv + PctKids2Par + PctWorkMom + PctIlleg + PctImmigRec5 + 
                     PctImmigRec8 + PctRecImmig5 + PctRecImmig8 + PctNotSpeakEnglWell + 
                     PctLargHouseOccup + PersPerOccupHous + PersPerRentOccHous + 
                     PctPersOwnOccup + PctPersDenseHous + HousVacant + PctHousOwnOcc + 
                     PctVacantBoarded + PctVacMore6Mos + RentLowQ + RentHighQ + 
                     MedRent + MedOwnCostPctIncNoMtg + NumInShelters + NumStreet + 
                     LemasPctOfficDrugUn,data =train2)

lm_stepsummary<-summary(lm_step_model)

modelmetrics2<-ModelBuilding(lm_step_model,lm_stepsummary,
                             train2,validation,'StepAIC-Both')
print(modelmetrics2)
modelmetrics<-rbind(modelmetrics,modelmetrics2)


##################################Model4-VIF on Step AIC model  ####################

library(car)
vifout<-data.frame(vif(lm_step_model))
names(vifout)<-c('vif')
row.names(vifout)
x<-subset(vifout,vifout$vif<10)
significant_col <- row.names(x)
print(significant_col)

lm_VIF_model<-lm(ViolentCrimesPerPop ~  racepctblack  +        pctUrban +            
                   pctWFarmSelf +         pctWRetire        +   
                  indianPerCap     +     AsianPerCap        +  
                   OtherPerCap     +      PctLess9thGrade   +   
                  PctEmplManu        +   PctWorkMom          + 
                   HousVacant        +    PctVacantBoarded    + 
                   PctVacMore6Mos     +   MedOwnCostPctIncNoMtg+
                   NumInShelters      +   NumStreet    +        
                  LemasPctOfficDrugUn,data =train2)

lm_VIF_Summary<-summary(lm_VIF_model)

modelmetrics2<-ModelBuilding(lm_VIF_model,lm_VIF_Summary,train2,validation,'VIF on StepAIC Model')
print(modelmetrics2)
modelmetrics<-rbind(modelmetrics,modelmetrics2)

##################################Model5-VIF on Model2  ################################

library(car)
vifout<-data.frame(vif(model2))
names(vifout)<-c('vif')
row.names(vifout)
x<-subset(vifout,vifout$vif<10)
significant_col <- row.names(x)
print(significant_col)

lm_VIF_model<-lm(ViolentCrimesPerPop ~   racePctAsian  +            pctUrban +pctWFarmSelf          +pctWRetire + blackPerCap    +       indianPerCap         
                 +AsianPerCap +          OtherPerCap +HispPerCap   +         PctUnemployed        
                 +PctEmplManu +          PctEmplProfServ      
                 +PctTeen2Par +          PctWorkMomYoungKids  
                 +NumImmig    +          PctImmigRecent       
                 + MedNumBR     +         PctHousOccup         
                 +PctVacantBoarded+      PctVacMore6Mos       
                 +MedYrHousBuilt  +      PctHousNoPhone       
                 +PctWOFullPlumb  +      MedRentPctHousInc    
                 +MedOwnCostPctInc +     MedOwnCostPctIncNoMtg
                 +NumInShelters    +     NumStreet            
                 +PctBornSameState  +    PctSameCity85        
                 +PctSameState85    +    LandArea             
                 +PopDens         +      PctUsePubTrans       
                 +LemasPctOfficDrugUn,data =train2)

lm_VIF_Summary<-summary(lm_VIF_model)

modelmetrics2<-ModelBuilding(lm_VIF_model,lm_VIF_Summary,train2,
                             validation,'VIF on Model2')
print(modelmetrics2)
modelmetrics<-rbind(modelmetrics,modelmetrics2)


###################################### Model5-PCA #################################

std_train=traindata  
std_validation=validationdata 
pca = princomp(std_train)
summary(pca)
plot(pca)
pca$loadings ##dimensions
pca$scores ##values(data) in the transformed dimensions 

#Checking how many components are really required from the point by observing the elbow point
screeplot(pca,type = 'lines',npcs=100)

##screeplot-Plot which helps in us to considering the number of components can be considered for anlaysis . The point where exactly the elbow is occuring is the point till which the information of data is described . After elbow point , it is the noise described and those components can be ignored for anlaysis .
##In our case , as we observe  sharp point at 9-10.  one at 9 and another 10 , let us build 2 models and verify the accuracy 

pca_train = predict(pca, std_train)
pca_validation = predict(pca, std_validation)

p.variance.explained = pca$sdev^2 / sum(pca$sdev^2)
barplot(100*p.variance.explained, las=2, xlab='', ylab='% Variance Explained')

###############   PCA- Components -9 #########################################
#Lets say 9 components
pca_X=data.frame(pca_train[,1:9],ViolentCrimesPerPop=train_target)
pca_V=data.frame(pca_validation[,1:9],ViolentCrimesPerPop=validation_target)

str(pca_X)

model3<-lm(ViolentCrimesPerPop~.,data=pca_X)
m3summary<- summary(model3)
modelmetrics3<-ModelBuilding(model3,m3summary,pca_X,pca_V,'PCA_9Components')
print(modelmetrics3)
modelmetrics<-rbind(modelmetrics,modelmetrics3)

###############PCA- Components -10 #########################################
#Lets say 10 components
pca_X=data.frame(pca_train[,1:10],ViolentCrimesPerPop=train_target)
pca_V=data.frame(pca_validation[,1:10],ViolentCrimesPerPop=validation_target)

str(pca_X)

model4<-lm(ViolentCrimesPerPop~.,data=pca_X)
m4summary<- summary(model4)
modelmetrics4<-ModelBuilding(model4,m4summary,pca_X,pca_V,'PCA_10Components')
print(modelmetrics4)
modelmetrics<-rbind(modelmetrics,modelmetrics4)

###############PCA- Components -11 #########################################
#Lets say 10 components-- 80% as threshold
pca_X=data.frame(pca_train[,1:11],ViolentCrimesPerPop=train_target)
pca_V=data.frame(pca_validation[,1:11],ViolentCrimesPerPop=validation_target)

str(pca_X)

model4<-lm(ViolentCrimesPerPop~.,data=pca_X)
m4summary<- summary(model4)
modelmetrics4<-ModelBuilding(model4,m4summary,pca_X,pca_V,'PCA_11Components')
print(modelmetrics4)
modelmetrics<-rbind(modelmetrics,modelmetrics4)



####################(LASSO /RIDGE/ELASTIC NET)###############################################
train_x<- as.matrix(traindata)
train_target<- as.matrix(train_target)
valid_x<-as.matrix(validationdata)
valid_target<-as.matrix(validation_target)

library(glmnet)
##################Lasso Regression  using glmnet - L1 norm########
lassofit <- glmnet(train_x,train_target, alpha=1)
coef(lassofit)
plot(lassofit,xvar="lambda",label=TRUE)

#Model Selection
cv.lasso=cv.glmnet(train_x,train_target) ##CV-Cross validation
plot(cv.lasso)
coef(cv.lasso) ##Only attributes which are considered as displayed . Feature selection happens
cv.lasso$lambda
log(cv.lasso$lambda.min)
log(cv.lasso$lambda.1se)

trainerror<-regr.eval(train_target, predict(cv.lasso,train_x))
validerror<-regr.eval(valid_target, predict(cv.lasso,valid_x))
modelmetrics4<-c(train_rmse=trainerror[2:3],validation_rmse=validerror[2:3])
modelmetrics<-rbind(modelmetrics,modelmetrics4)


####################Ridge Regression  using glmnet  - L2 norm#########
ridgefit <- glmnet(train_x,train_target,alpha=0)
plot(ridgefit,xvar="lambda",label=TRUE)

#Model Selection
coef(ridgefit)
cv.ridge=cv.glmnet(train_x,train_target,alpha=0)
plot(cv.ridge)
coef(cv.ridge)

trainerror<-regr.eval(train_target, predict(cv.ridge,train_x))
validerror<-regr.eval(valid_target, predict(cv.ridge,valid_x))

modelmetrics4<-c(train_rmse=trainerror[2:3],validation_rmse=validerror[2:3])
modelmetrics<-rbind(modelmetrics,modelmetrics4)

#################### Elastic Net Regression  using glmnet ############
elasticfit <- glmnet(train_x,train_target,alpha=0.5)
plot(elasticfit,xvar="lambda",label=TRUE)

#Model Selection
coef(elasticfit)
cv.elasticfit=cv.glmnet(train_x,train_target,alpha=0)
plot(cv.elasticfit)
coef(cv.elasticfit)

trainerror<-regr.eval(train_target, predict(cv.elasticfit,train_x))
validerror<-regr.eval(valid_target, predict(cv.elasticfit,valid_x))

modelmetrics4<-c(train_rmse=trainerror[2:3],validation_rmse=validerror[2:3])
modelmetrics<-rbind(modelmetrics,modelmetrics4)

Description<- c('LinearModel','lm_OutliersandLeveragesRemoved',
                'StepAIC','VIF on StepAIC Model',
                'VIF on Model2','PCA_9Components',
                'PCA_10Components','PCA_11Components',
                'LASSO','RIDGE','ELASTIC NET')
##modelmetrics<-cbind(modelmetrics,description)
row.names(modelmetrics)<-Description

# row.names(modelmetrics)<- c('Model1','Model2','Model3','Model4',
#                             'Model5','Model6','Modle7','Model8',
#                             'Model9','Modle10','Model11')

