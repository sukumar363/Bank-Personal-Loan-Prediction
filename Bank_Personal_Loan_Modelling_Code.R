rm(list = ls())
#In Mac, We have set the working directory from Session --> Set Working Directory



BankLoan=read.csv("Bank_Personal_Loan_Modelling.csv", header=TRUE)
dim(BankLoan)
head(BankLoan)
str(BankLoan)
any(is.na(BankLoan))
BankLoan=subset(BankLoan,select=-c(ID,ZIP.Code))
head(BankLoan)
fc=c("Education","Personal.Loan","Securities.Account", "CD.Account", "Online", "CreditCard")
BankLoan[fc] = lapply(BankLoan[fc], factor)
str(BankLoan)
head(BankLoan)
levels(BankLoan$Education)
levels(BankLoan$Personal.Loan)
summary(BankLoan) #checking for outliers,data entry errors
BankLoan$Experience=abs(BankLoan$Experience)
summary(BankLoan)
#install.packages("ggplot2")
library(ggplot2)
library(readr)
library(dplyr)
#install.packages("DataExplorer")
library(DataExplorer)
head(BankLoan)
age<-ggplot(BankLoan, aes(x=Age)) +
  geom_histogram(color="black", fill="light blue")
age



library(purrr)
library(tidyr)



BankLoan %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(color="black", fill="slateblue")



plot_boxplot(BankLoan, by = "Personal.Loan",
             geom_boxplot_args = list("outlier.color" = "Purple"))



head(BankLoan)
by_loan<-BankLoan %>% group_by(Personal.Loan)
head(by_loan)
averages<-by_loan %>% summarise(avg_age = mean(Age),avg_experience = mean(Experience), avg_income=mean(Income), avg_size=mean(Family))
ggplot(data=averages, aes(x=Personal.Loan, y=avg_age)) +
  geom_bar(stat="identity", fill="slateblue")+
  geom_text(aes(label=avg_age), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
ggplot(data=averages, aes(x=Personal.Loan, y=avg_experience)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=avg_experience), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
ggplot(data=averages, aes(x=Personal.Loan, y=avg_income)) +
  geom_bar(stat="identity", fill="slateblue")+
  geom_text(aes(label=avg_income), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
ggplot(data=averages, aes(x=Personal.Loan, y=avg_size)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=avg_size), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
a1 = ggplot(BankLoan, aes(Income, fill= Personal.Loan)) + geom_density(alpha=0.4)+ylab("Income_Density")
a2 = ggplot(BankLoan, aes(Mortgage, fill= Personal.Loan)) + geom_density(alpha=0.4)+ylab("Mortgage_Density")
a3 = ggplot(BankLoan, aes(Age, fill= Personal.Loan)) + geom_density(alpha=0.4)+ylab("Age_Density")
a4 = ggplot(BankLoan, aes(Experience, fill= Personal.Loan)) + geom_density(alpha=0.4)+ylab("Experience_Density")
a5 = ggplot(BankLoan, aes(Education, fill= Personal.Loan)) + geom_histogram(alpha=0.4,stat="count")+ylab("Education_frequency")
a6 = ggplot(BankLoan, aes(Income, Mortgage, color = Personal.Loan))+ geom_point(alpha = 0.7)
library(gridExtra)
grid.arrange(a1,a2,a3,a4,a5,a6, ncol = 2, nrow = 3)



b5=ggplot(BankLoan, aes(Income,y = CCAvg, color = Personal.Loan)) +
  geom_point(size = 1)



b1=ggplot(BankLoan, aes(Securities.Account,fill= Personal.Loan)) + geom_bar(stat = "count", position = "dodge") +theme_minimal()
b2=ggplot(BankLoan, aes(CD.Account,fill= Personal.Loan)) + geom_bar(stat = "count", position = "dodge") +theme_minimal()
b3=ggplot(BankLoan, aes(Online,fill= Personal.Loan)) + geom_bar(stat = "count", position = "dodge") +theme_minimal()
b4=ggplot(BankLoan, aes(CreditCard,fill= Personal.Loan)) + geom_bar(stat = "count", position = "dodge") +theme_minimal()
grid.arrange(b1,b2,b3,b4,b5, ncol = 2, nrow = 3)



#EDA end



fc=c("Education","Securities.Account", "CD.Account", "Online", "CreditCard")



BankLoan[fc] = lapply(BankLoan[fc], factor)
BankLoanclus=BankLoan %>% select_if(is.numeric)
BankLoan.sc = scale(BankLoanclus, center = TRUE)
head(BankLoan.sc)
#install.packages("factoextra")
library(cluster)
seg.dist<-daisy(BankLoan.sc)
as.matrix(seg.dist)
seg.hc<-hclust(seg.dist, method="complete")
plot(seg.hc)
plot(cut(as.dendrogram(seg.hc), h=7))
install.packages("factoextra")
library(factoextra)
clus_plot1 = fviz_nbclust(BankLoan.sc, kmeans, method = "silhouette", k.max = 5)
clus_plot2 = fviz_nbclust(BankLoan.sc, kmeans, method = "wss", k.max = 5)



grid.arrange(clus_plot1, clus_plot2, ncol=2)
plot(rect.hclust(seg.hc, k=3, border="red"))
set.seed(1000)
BankLoan.k<-kmeans(BankLoan.sc, centers=3)
fviz_cluster(BankLoan.k, BankLoan.sc, geom = "point", ellipse = TRUE, pointsize = 0.4, ) + theme_minimal()
BankLoan.k$cluster
seg.summ<-function(data,groups){
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}
seg.BankLoan.num<-BankLoan
seg.BankLoan.num$Personal.Loan <- ifelse(BankLoan$Personal.Loan==0, 0, 1)
seg.BankLoan.num$Securities.Account <- ifelse(BankLoan$Securities.Account==0, 0, 1)
seg.BankLoan.num$CD.Account <- ifelse(BankLoan$CD.Account==0, 0, 1)
seg.BankLoan.num$Online <- ifelse(BankLoan$Online==0, 0, 1)
seg.BankLoan.num$CreditCard <- ifelse(BankLoan$CreditCard==0, 0, 1)
seg.summ(seg.BankLoan.num, BankLoan.k$cluster)



# Segmentation over
# Classification Starts
## sampling 70% of data for training the algorithms using random sampling
set.seed(1000)
perc_train = sample(1:nrow(BankLoan), nrow(BankLoan)*0.80)
BankLoan_train = BankLoan[perc_train,]
BankLoan_test = BankLoan[-perc_train,]



dim(BankLoan_test)
set.seed(1000)
library(rpart)
dt = rpart(Personal.Loan~., data = BankLoan_train, method = "class")
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(dt)
dt$variable.importance
install.packages("GPArotation")
install.packages("gmodels")
library(gmodels)
pred.cart<-predict(dt, newdata=BankLoan_test,type="class")
Cross1=CrossTable(BankLoan_test$Personal.Loan, pred.cart)
#install.packages("caret")
library(caret)
confusionMatrix(table(BankLoan_test$Personal.Loan,pred.cart ))




#Random Forest

library(nFactors)
library(psych)
library(GPArotation)
eigen(cor(BankLoan[,1:5]))
library(randomForest)
rf <- randomForest(Personal.Loan ~ ., ntree = 500, nodesize=10,data = BankLoan_train, importance = TRUE)
rf
Pred_rf <- predict(rf, BankLoan_test, type = 'class')
confusionMatrix(BankLoan_test$Personal.Loan, Pred_rf)
varImpPlot(rf, sort = T)
#using the model for predicting a given input
#pred_rf.pred <- predict(pred_rf,data.frame(''))



#Logistic Regression
fit.logit <- glm(Personal.Loan ~ . ,data=BankLoan_train,family=binomial("logit"))
pred.valid=predict(fit.logit, BankLoan_test[,-8],type="response" )
pred.valid <- ifelse(pred.valid> 0.5,1,0)
(ctv=table(BankLoan_test[,8], pred.valid))




#Neuralnets



library(neuralnet)
library(nnet)
library(caret)



indx <- sapply(BankLoan_train, is.factor)
BankLoan_train[indx] <- lapply(BankLoan_train[indx], function(x) as.numeric(as.character(x)))



nn<-neuralnet(Personal.Loan ~ . ,
              data=BankLoan_train,hidden = c(4,2),linear.output = FALSE)
plot(nn)





preds<-compute(nn,BankLoan_train[,-8])
preds.class<-ifelse(preds$net.result>0.5,1,0)
confusionMatrix(as.factor(preds.class),as.factor(BankLoan_train$Personal.Loan))



indx2 <- sapply(BankLoan_test, is.factor)
BankLoan_test[indx2] <- lapply(BankLoan_test[indx2], function(x) as.numeric(as.character(x)))



preds.valid<-compute(nn,BankLoan_test[,-8])
preds.valid.class<-ifelse(preds.valid$net.result>0.5,1,0)
confusionMatrix(as.factor(preds.valid.class),as.factor(BankLoan_test[,8]))




# Ensemble using Majority Voting
BankLoan_test$pred_majority<-as.factor(ifelse(Pred_rf=='1' & pred.cart=='1','1',
                                              ifelse(Pred_rf=='1' & preds.valid.class=='1','1',
                                                     ifelse(pred.cart=='1' & preds.valid.class=='1','1','0'))))
ensemble.voting<-confusionMatrix(as.factor(BankLoan_test$Personal.Loan),as.factor(BankLoan_test$pred_majority))
ensemble.voting





# Ensemble using Weighted Average
# Taking weighted average of predictions
BankLoan_test$pred_weighted<-(as.numeric(Pred_rf)*0.25)+(as.numeric(pred.cart)*0.25)+(as.numeric(preds.valid.class)*0.5)
#Splitting into binary classes at 0.5
BankLoan_test$pred_weighted<-as.factor(ifelse(BankLoan_test$pred_weighted>0.5,'1','0'))
ensemble.weighted<-confusionMatrix(as.factor(BankLoan_test$Personal.Loan),as.factor(BankLoan_test$pred_weighted))
ensemble.weighted