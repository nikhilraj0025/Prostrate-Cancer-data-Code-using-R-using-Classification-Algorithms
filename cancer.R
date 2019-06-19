pro<-read.csv("C:/Users/AKHIL/Desktop/New folder/Prostate_Cancer.csv")
View(pro)
library(dplyr)
pro<-select(pro,2,3,4,5,6,7,8,9,10)
View(pro)
pro<-mutate(pro,diagnosis_result1=ifelse(diagnosis_result=="M",1,0))
View(pro)
pro<-select(pro,3,4,5,6,7,8,9,10)
View(pro)

d<-table(pro$diagnosis_result1)
d
pro$diagnosis_result1=factor(pro$diagnosis_result1)


set.seed(121)
crd<-sample(2,nrow(pro),replace=TRUE,prob=c(0.8,0.2))
pro_Train<-pro[crd==1,]
pro_Test<-pro[crd==2,]

###############logistic############################
model_pro1<-glm(diagnosis_result1~.,family=binomial,data=pro_Train)
summary((model_pro1))
prod<-predict(model_pro1,pro_Test,type="response")
prod_df<-data.frame(prod,pro_Test$diagnosis_result1)
prod_df<-mutate(prod_df,prod=ifelse(prod>0.5,1,0))
colnames(prod_df)<-c("predict","actual")
tab14<-table(prod_df$predict,prod_df$actual)   #####85.71 accuracy
tab14
accuracy14<-sum(diag(tab1))/sum(tab1)
accuracy14



######decision tree#########################################
library(party)
model_pro2<-ctree(diagnosis_result1~.,data=pro_Train)
prod1<-predict(model_pro2,pro_Test,type="response")
prod_df1<-data.frame(prod1,pro_Test$diagnosis_result1)
colnames(prod_df1)<-c("predict","actual")
tab13<-table(prod_df1$predict,prod_df1$actual)   #####85.71 accuracy
tab13
accuracy13<-sum(diag(tab13))/sum(tab13)
accuracy13



###########################random forest###############################
library(randomForest)
model_pro3<-randomForest(diagnosis_result1~.,data=pro_Train)
prod3<-predict(model_pro3,pro_Test,type="response")
prod_df3<-data.frame(prod3,pro_Test$diagnosis_result1)
colnames(prod_df3)<-c("predict","actual")
tab15<-table(prod_df3$predict,prod_df3$actual)   #####85.71 accuracy
tab15
accuracy15<-sum(diag(tab15))/sum(tab15)
accuracy15





############### model on naive Bayes on Test #######################################


library(e1071)
model_pro4<-naiveBayes(diagnosis_result1~.,data=pro_Train)
prod4<-predict(model_pro4,pro_Test)
prod_df4<-data.frame(prod4,pro_Test$diagnosis_result1)
colnames(prod_df4)<-c("predict","actual")
tab18<-table(prod_df4$predict,prod_df4$actual)   #####85.71 accuracy
tab18
accuracy18<-sum(diag(tab18))/sum(tab18)
accuracy18

