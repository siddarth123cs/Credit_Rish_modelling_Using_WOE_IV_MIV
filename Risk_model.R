rm(list=ls(all=T))
setwd("D:\\Koino\\Koino_scorecard\\Modelling")
master_data=read.csv("trainset.csv",stringsAsFactors = F)
master_data=master_data[!master_data$score<11,]

master_data$new_cd_closure=ifelse((master_data$cd_trades_with_good_closure>0 & master_data$cd_loan_count_Last_24_mon>0)==T , (master_data$cd_trades_with_good_closure/master_data$cd_loan_count_Last_24_mon),master_data$cd_trades_with_good_closure)
master_data_fr=data.frame(master_data[!duplicated(master_data$bureau_report_reference_number),]) #%>%filter(Merchant.Channel=="FRANCHISEE") %>%
#Removing unnecessary variables
master_data_fr[,c("X","bureau_report_reference_number","User.Id","Fb.Transaction.Id")]=NULL
#Converting all Negatives to "-1"
master_data_fr[is.na(master_data_fr)]=-1
master_data_fr[master_data_fr<0]=-1

rm(master_data)
#Solving For MIV
library(miv.select)
library(dplyr)
library(purrr)
library(mgcv)
library(rsample)
library(caret)
library(woeBinning)
library(scorecard)

set.seed(100)
#Binning using WOE
bin=woebin(master_data_fr, "t_10")
master_Data_binned = woebin_ply(master_data_fr, bin)
#Splitting the dataset
Train_ID=createDataPartition(master_Data_binned$t_10,p=0.8,list = F)

#original train and test
train_non_factor=data.frame(master_Data_binned[Train_ID,])
test_non_factor=data.frame(master_Data_binned[-Train_ID,])
master_Data_binned=data.frame(lapply(master_Data_binned, function(x) as.factor(x)))

#Train and Test with factored variables
train=data.frame(master_Data_binned[Train_ID,])
test=data.frame(master_Data_binned[-Train_ID,])

#Binning the features for testing for the MIV
library(scorecard)
binned_features <-bin_all(train, y = "t_10", bins=5, verbose = FALSE)
colnames(master_data_fr[,setdiff(names(master_data_fr),"t_10")])=paste(colnames(master_data_fr[,setdiff(names(master_data_fr),"t_10")]),"woe",sep = "_")

#Pulling out the IV scores from binned features
for (i in 1:length(binned_features)) {
  Variable=binned_features[[i]]$feature
  IV=as.numeric(binned_features[[i]]$iv)
  row=cbind(Variable,IV)
  if(i==1)
  {IV_Data=data.frame(row)}
  else {IV_Data=data.frame(rbind(IV_Data,row))}
}

IV_Data$IV=as.numeric(as.character(IV_Data$IV))
IV_Data$Variable=as.character(IV_Data$Variable)
IV_Data=IV_Data[order(-IV_Data$IV),]
Model_features=c("t_10",IV_Data$Variable[1])

###########plot#########
#Enter the most important Feature
# feature="Digi_avg_balance_3_month"
# plot(binned_features[[feature]], train, y = 'Digi_TwoNSin5_final')
# table(train$Digi_avg_balance_3_month)
####################
#Building model with the required features
#Enter the number of variables you want to consider
n=45
auc=rep(0,n)
for (i in 1:n) {
  print(i)

  # transform_features <- function(df){
  #   df %>%
  #     mutate(overall_trades_good_closuer_loan=overall_trades_good_closuer_loan %>% replace(.>20,20),
  #   Overall_enquiries_in_last_3_mon=Overall_enquiries_in_last_3_mon %>% replace(.>7,7),
  #   cc_hca_Last_24_mon=cc_hca_Last_24_mon %>% replace(.>0,1),
  #   cd_trades_with_30_del_or_worse_ever=factor(cd_trades_with_30_del_or_worse_ever),
  #   non_agri_avg_age_trade_Last_6_mon=non_agri_avg_age_trade_Last_6_mon %>% replace(.>13,13) )
  #   }
  #
  # transformed_train <- train %>% transform_features()
  # transformed_test<- test %>% transform_features()
  assign(paste("candidate_model",i,sep = "_") , glm(
    t_10~.,data = (train_non_factor[,names(train[,(names(train) %in% Model_features)==T])]),

    family = "binomial"
  ))

  candidate_model= glm(
    t_10~.,data = (train_non_factor[,names(train[,(names(train) %in% Model_features)==T])]),

    family = "binomial"
  )

  summary(candidate_model)
  #Model Results on Test Data
  model_predictions <- predict(candidate_model, train_non_factor, type = 'response')
  validation_predictions <- predict(candidate_model, test_non_factor, type='response')
  validation_predictions_confusion=as.factor(ifelse(validation_predictions>=0.5,1,0))

  #Printing the confusion matrix
  if(length(levels(validation_predictions_confusion))>1){
    print(confusionMatrix(validation_predictions_confusion,test$t_10))
  }
  auc[i]=auc(validation_predictions, test$t_10)

  #Calculating the MIV's


  # if(i==1 | auc[i]>=auc[i+1]){
  all_mivs <- miv.select::calculate_all_mivs(train, y = 't_10', model_predictions)
  all_mivs=all_mivs[((all_mivs$feature_name %in% Model_features)==F),]
  Model_features=c(Model_features,(all_mivs$feature_name[1]))
  #                           } else {break}

}
summary(candidate_model)
max(auc*2)-1

#Features used in the model
Model_features

# Calculating the VIF
library(car)
car::vif(model)

# write.csv(cor(master_data[,setdiff(names(master_data),"t_10")]),"Correlation_plot.csv")
library(glmnet)
model=glm(t_10~.,master_data_binning,family = "binomial")
summary(model)
#########################################################################################################################
# MODEL
#########################################################################################################################
# master_data=master_data[, names(master_data) %in% c( "t_10"                                   ,"overall_trades_good_closuer_loan"       ,"Overall_enquiries_in_last_24_mon"      
#                            ,"overall_hca_Last_9_mon"                 ,"cc_satisfactory_avg_age_trade"          ,"secured_open_overall_total_outstanding"
#                            ,"cd_enquiries_in_last_24_mon"            ,"cd_oldest_trade_age_Last_3_mon"         ,"pl_satisfactory_avg_age_trade"         
#                            ,"score"                                  ,"payday_overall_oldest_trade_age"        ,"new_cd_closure"                        
#                            ,"secured_overall_hca"                    ,"unsecured_overall_utilization_rate"     ,"unsecured_enquiries_in_last_6_mon"     
#                            ,"cc_overall_loan_count"                  ,"max_DPD_Bucket"                         ,"overall_oldest_trade_age_Last_24_mon"  
#                            ,"payday_avg_age_trade_Last_12_mon"       ,"cd_satisfactory_oldest_trade_age"       ,"non_agri_avg_age_trade_Last_9_mon"     
#                            ,"non_agri_month_since_90_del"            ,"pl_enquiries_in_last_6_mon"             ,"cc_enquiries_in_last_24_mon"           
#                            ,"cc_overall_hca"                         ,"cc_oldest_trade_age_Last_9_mon"         ,"pl_avg_age_trade_Last_24_mon"          
#                            ,"cd_oldest_trade_age_Last_24_mon"        ,"X_DPD_L9M_Overall"                      ,"secured_utilization_rate_Last_24_mon"  
#                            ,"non_agri_avg_age_trade_Last_24_mon"     ,"pl_avg_age_trade_Last_12_mon"           ,"non_agri_trades_with_good_closure"     
#                            ,"secured_enquiries_in_last_3_mon"        ,"non_agri_open_trades_with_balance_per"  ,"non_agri_youngest_trade_age_Last_6_mon"
#                            ,"secured_month_since_30_del_or_worse"    ,"overall_hca"                            ,"DPD_bucket.x"                          
#                            ,"pl_oldest_trade_age_Last_12_mon"        ,"overall_utilization_rate_Last_6_mon"    ,"cd_overall_avg_age_trade"              
#                            ,"pl_satisfactory_oldest_trade_age"       ,"pl_trades_with_good_closure"           
#                            ,"secured_overall_loan_count"             ,"non_agri_oldest_trade_age_Last_24_mon")]
Test=read.csv("All_metrics_var.csv",stringsAsFactors = F)
Test=Test[!Test$score<11,]
master_data=master_data[, names(master_data) %in% c( "t_10" ,"overall_trades_good_closuer_loan"       #,"Overall_enquiries_in_last_24_mon"      
                          ,"overall_hca_Last_9_mon"                 ,"cc_satisfactory_avg_age_trade","new_cd_closure"
                          ,"cc_overall_loan_count"                  ,"max_DPD_Bucket"           
                          ,"secured_utilization_rate_Last_24_mon"
                          ,"pl_oldest_trade_age_Last_12_mon"        #,"cd_overall_avg_age_trade"
                          ,"secured_overall_loan_count"             )]
Test=Test[, names(Test) %in% c( "t_10" ,"overall_trades_good_closuer_loan"       #,"Overall_enquiries_in_last_24_mon"      
                                                     ,"overall_hca_Last_9_mon"                 ,"cc_satisfactory_avg_age_trade","new_cd_closure"
                                                     ,"cc_overall_loan_count"                  ,"max_DPD_Bucket"           
                                                     ,"secured_utilization_rate_Last_24_mon"
                                                     ,"pl_oldest_trade_age_Last_12_mon"        #,"cd_overall_avg_age_trade"
                                                     ,"secured_overall_loan_count"             )]


str(master_data)
master_data[is.na(master_data)]=-1
master_data[master_data<0]=-1

Test[is.na(Test)]=-1
Test[Test<0]=-1
library(caret)
library(doParallel)
#Train_ID=createDataPartition(master_data$t_10,p=0.8,list = F)
#original train and test
master_data$t_10=as.factor(master_data$t_10)
Test$t_10=as.factor(Test$t_10)



#master_data_train=data.frame(master_data[Train_ID,])
#master_data_test=data.frame(master_data[-Train_ID,])
#train_control <- trainControl(method="cv",number = 10)
train_control <-
  trainControl(
    method = "cv",
    number = 10,
    classProbs = T,
    savePredictions = T,
    summaryFunction = twoClassSummary
  )

binning=woebin(master_data,"t_10")
# for (i in 1:length(binning)) {
#   
#   Variable=binning[[i]]$variable
#   IV=as.numeric(binning[[i]]$total_iv)
#   row=cbind(Variable,IV)
#   if(i==1)
#   {IV_Data=data.frame(row)}
#   else {IV_Data=data.frame(rbind(IV_Data,row))}
# }
master_data_binning=woebin_ply(master_data,binning)
Test_binning=woebin_ply(Test,binning)
master_data_binning$t_10=ifelse(master_data_binning$t_10==1,"BAD","GOOD")
Test$t_10=ifelse(Test$t_10==0,"GOOD","BAD")

registerDoSEQ()
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
#master_data_binning=data.frame(apply(master_data_binning , MARGIN=2, function(x) as.numeric(x)))
model <- train(t_10~., data=master_data_binning, trControl=train_control, method="glm",family = "binomial",metric="ROC")

library(plyr)
library(MLmetrics)
model$pred[2]=ifelse(model$pred[2]=="BAD",1,0)
ddply(model$pred, "Resample", summarise,
      accuracy = auc(obs, BAD))
# master_data_binning_test=woebin_ply(master_data_test,binning)
# test=predict(model,master_data_binning_test,type="prob")
# confusionMatrix(test,as.factor(master_data_binning_test$t_10))
print(model)
# write.csv(cbind(master_data_test,test),"temp.csv")
write.csv(cbind(master_data_binning,predict(model,master_data_binning,type = 'prob')),"train_2.csv")

write.csv(cbind(Test,predict(model,Test_binning,type = 'prob')),"Test.csv")


# Printing out the correlation matrix into excel
#write.csv(cor(data.frame(master_data_fr[,(names(master_data_fr) %in% Model_features)])),"Correlation.csv")
summary(model)


#Selecting columns for binning purpose
master_variable_for_binning <- Model_data# 
master_variable_for_binning$attention_to_duration_Binary= ifelse(master_variable_for_binning$attention_to_duration>0.1,1,0)
# ## WOE bins calculation:
woe_bins_adjusted = woebin(master_variable_for_binning, y="attention_to_duration_Binary", method="tree")
## save binning plot
plots_demog=woebin_plot(woe_bins_adjusted, x = NULL, title = NULL, show_iv = TRUE)
setwd("C:\\Users\\G041\\Desktop\\STAR\\Data\\Bivariate_Latest")
for (i in 1:length(plots_demog)) {
  ggplot2::ggsave(
    paste0(names(plots_demog[i]), ".png"), plots_demog[[i]],
    width = 25, height = 25, units="cm" )
}
# 
# library(ROCR)
# auc=rep(0:10)
# for (i in 1:10) {
#   master_data_binning$row=1:nrow(master_data_binning)
#   Test=master_data_binning[((i-1)*280+1):(i*280),]
#   Train=master_data_binning[!(master_data_binning$row %in% Test$row),]
#   model=glm(t_10~.,Train[,-12],family = "binomial")
#   validation_predictions=predict(model,Test[,-12],type = "response")
#   auc[i]=glmnet::auc(validation_predictions, Test$t_10)
# }
# ?auc
