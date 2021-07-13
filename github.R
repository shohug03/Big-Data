getwd()
setwd("C:/Users/morshad/Desktop/Project")
my_data=read.csv("SBAnational.csv", header=T)

LoanNo=my_data$LoanNr_ChkDgt
Name=my_data$Name
City=my_data$City
State=my_data$State
Zip=my_data$Zip
BankName=my_data$Bank
BankState=my_data$BankState
NAICS=my_data$NAICS
AppDate=my_data$ApprovalDate
AppFY=my_data$ApprovalFY
LoanTerm=my_data$Term
NumbrEmp=my_data$NoEmp
NewExist=my_data$NewExist
JobCreated=my_data$CreateJob
JobRetained=my_data$RetainedJob
FrnchiseCode=my_data$FranchiseCode
UrbanRural=my_data$UrbanRural
RevLineCr=my_data$RevLineCr
LowDoc=my_data$LowDoc
ChgoffDate=my_data$ChgOffDate
DisbursementDate=my_data$DisbursementDate
DisbursementGross=my_data$DisbursementGross
BalanceGross=my_data$BalanceGross
MIS_status=my_data$MIS_Status
ChgOffPrinGr=my_data$ChgOffPrinGr
GrAppv=my_data$GrAppv
SBA_Appv=my_data$SBA_Appv

#mydata=data.frame(LoanNo,Name,City,State,Zip,BankName,BankState,NAICS,AppDate,AppFY,LoanTerm,NumbrEmp,NewExist,JobCreated,JobRetained,FrnchiseCode,UrbanRural,RevLineCr,LoxDoc,ChgoffDate,
                  DisbursementDate,DisbursementGross,BalanceGross,MIS_status,ChgOffPrinGr,GrAppv,SBA_Appv)
#summary(mydata)
#str(mydata)

#install.packages("VIM")
#library(VIM)
#mydata_1 = kNN(mydata,variable = "NewExist", k=3)
#summary(mydata_1)

#Creating the data frame// selecting only 9 variables from the 27 attributes
mydata_1 = data.frame(AppFY,LoanTerm,NumbrEmp,NewExist,JobCreated,JobRetained,UrbanRural,DisbursementGross,MIS_status)
str(mydata_1)

#shufflng the data 
mydata_1=mydata_1[sample(nrow(mydata_1)),]
head(mydata_1)

#-------------------------------------------------------------------------------------

#Hypothesis testing

#-------------------------------------------------------------------------------------

#visualisation
#barplot (frequency of the loan that is charged off and paid in full)
class(MIS_status)
library(plyr)

opt=count(MIS_status)
cf=opt$freq
labels=opt$x
opt
barplot(cf,main="Class frequency of Loan approved and loan not approved",names.arg=labels,col = rainbow(length(cf)))

#pie chart for loan distributin in Urban/ Rural and undefined areas
opt1=count(UrbanRural)
cf1=opt1$freq
labels1=opt1$x
crf=table(my_data$UrbanRural)/nrow(my_data)
piepercent=round(100*crf/sum(crf),2)
crf
pie(crf,label=piepercent,main="Pie Chart for Type of Area", col = rainbow(length(crf)))
legend("topright", c("0","1","2"),cex=0.8,fill=rainbow(length(crf)))


#histogram of the term of loans in months
h=hist(LoanTerm)
xfit<-seq(min(LoanTerm),max(LoanTerm),length=100) 
yfit<-dnorm(xfit,mean=mean(LoanTerm),sd=sd(LoanTerm))
yfit <- yfit*diff(h$mids[1:2])*length(LoanTerm)
lines(xfit, yfit, col="blue", lwd=2)


#-------------------------------------------------------------------------------------

#K-nearest Neighbor
#Preprocessing for KNN model
knn_data=mydata_1
knn_data$NewExist=as.character(knn_data$NewExist)
library(plyr)
knn_data$NewExist=revalue(knn_data$NewExist, c("1"= "0"))
knn_data$NewExist=revalue(knn_data$NewExist, c("2"= "1"))
knn_data$NewExist=as.integer(knn_data$NewExist)
library(dummies)
knn_data = dummy.data.frame(knn_data,names="UrbanRural")
knn_data$MIS_status=revalue(knn_data$MIS_status, c("CHGOFF"= "0"))
knn_data$MIS_status=revalue(knn_data$MIS_status, c("P I F"= "1"))
str(knn_data)

#Scalling the preprocessed knn data
#extracting numerical data
num.vars.knn <- sapply(knn_data, is.numeric)

#normalizing the knn data
knn_data[num.vars.knn] = lapply(knn_data[num.vars.knn],scale)
head(knn_data)

#dividing the pre-processed data into training and testing
set.seed(1)
train=1:716804
knn.train.data <- knn_data[train,]          #train data
knn.tst.data = knn_data[-train,]            #test data
str(knn.train.data)
str(knn.tst.data)
head(knn.train.data)
head(knn.tst.data)

#dividing the label in training and testing
knn.train.MIS=knn.train.data$MIS_status
knn.tst.MIS=knn.tst.data$MIS_status

#knn model building at K=200,300,499 with the training data
library(class)
knn.200 = knn(knn.train.data,knn.tst.data,knn.train.MIS,k=200)
knn.300 = knn(knn.train.data,knn.tst.data,knn.train.MIS,k=300)
knn.499 = knn(knn.train.data,knn.tst.data,knn.train.MIS,k=499)

#finding the accuracy of the models built on testing data
accuracy(knn.tst.MIS, knn.200)
accuracy(knn.tst.MIS, knn.300)
accuracy(knn.tst.MIS, knn.499)

#-------------------------------------------------------------------------------------

#Naive Baye's
#shuffled raw data
naive_data=mydata_1

#pre-processing the data
naive_data[,1] = cut(naive_data[,1], breaks = c(1968,2007,2009,2014),
                           labels = c("non-recession1", "recession","non-recession2"))
naive_data[,2] = cut(naive_data[,2], breaks = c(-1,60,84,120,569),
                           labels = c("very-short term", "short-term","long-term","very-long term"))
naive_data[,3] = cut(naive_data[,3], breaks = c(-1,2500,7500,9999),
                           labels = c("small-sized","medium-sized","large-sized"))
naive_data[,4] = factor(naive_data[,4])
naive_data$JobCreated = cut(naive_data$JobCreated,3)
naive_data[,6] = cut(naive_data[,6],3)
naive_data[,7] = factor(naive_data[,7])
naive_data[,8] = cut(naive_data[,8], breaks = c(3999,42400,10000,238390,11446325),
                           labels = c("very-low-requirment","low-requirement","medium-requirement","high-requirement"))
str(naive_data)

#dividing the pre-processed data into training and testing
set.seed(1)
naive.train.data <- naive_data[train,]          #train data
naive.tst.data = naive_data[-train,]            #test data
str(naive.train.data)
str(naive.tst.data)
head(naive.train.data)
head(naive.tst.data)

#naive base model with the training data
library(naivebayes)
nb_model=naive_bayes(MIS_status ~ .,naive.train.data)
nb_model

#finding the accuracy of the models built on testing data
pred=predict(nb_model, naive.tst.data)
pred
head(predict(nb_model, naive.tst.data, type = "prob"))
library(Metrics)
accuracy(naive.tst.data[,9], pred)


#-------------------------------------------------------------------------------------


