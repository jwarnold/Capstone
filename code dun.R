#Packages


install.packages("randomForest")
install.packages("caret") #Classification And REgression Training
install.packages("party")
install.packages("ISLR")
install.packages("corrplot")
install.packages("ROCR")
install.packages("tree")
install.packages("RWeka")
install.packages("e1071") #svmachine
install.packages("rpart")
install.packages("pROC") #ROC curves
install.packages("C50")# C50 tree
install.packages("printr") #C50 tree

library(randomForest)
library(caret)
library(party)
library(ISLR)
library(corrplot)
library(ROCR)
library(tree)
library(RWeka)
library(rpart)
library(e1071)
library(pROC)
library(C50)
library(printr)




################                ################                ################                ################               

#load

df<-data.frame(read.csv("C:/Users/jatosan/Desktop/CAPSTONE/semifinal/secondtry/2500_column_gone_2016Q1.csv"))


################                ################                ################                ################

#Clean up

#Removing "current" rows
df<-subset(df,!(df$loan_status=="Current"))


###Outlier removal
o1<-which(df$tot_cur_bal>=600000)
o2<-which(df$tot_hi_cred_lim>750000)
o3<-which(df$total_il_high_credit_limit>200000)
o4<-which(df$total_rev_hi_lim>150000)

o.all<-c(o1,o2,o3,o4)
outliers<-unique(o.all)

#removal from df
df<-df[-c(outliers),]



#Coerce lates, grace periods and charge offs to 1 and fully paid to 0

#Get loan status col name index
grep("loan_status", colnames(df))

#change class of loan status to numeric to coerce loan status attributes

gracep<-which(df$loan_status=="In Grace Period")
late1<-which(df$loan_status=="Late (16-30 days)")
late2<-which(df$loan_status=="Late (31-120 days)")
default<-which(df$loan_status=="Charged Off")

loanstat1<-c(gracep,late1,late2,default)

fpaid<-which(df$loan_status=="Fully Paid")

loanstat0<-fpaid

#change class of loan status to numeric to coerce
df$loan_status<-as.numeric(df$loan_status)
#Coercing
df[loanstat1,12]<-1
df[loanstat0,12]<-0

#Change class back to factor for classification models

df$loan_status<-as.factor(df$loan_status)

#Remove percentage sign in df columns (int_rate, revol_util) and change to numeric for correlation matrix

int_rateclean<-as.numeric(gsub("%","",df$int_rate))
df$int_rate<-int_rateclean

revol_utilcleann<-as.numeric(gsub("%","",df$revol_util))
df$revol_util<-revol_utilcleann

#Changing emp_length to numeric values

emp_lengthc1<-gsub(" years | year|s|+","",df$emp_length)
emp_lengthc2<-sub("10+","10",emp_lengthc1,fixed = TRUE)
emp_lengthc3<-sub("< 1","0",emp_lengthc2,fixed = TRUE)
emp_lengthc4<-sub("n/a","0",emp_lengthc3,fixed = TRUE)
emp_lengthc5<-as.numeric(emp_lengthc4)

df$emp_length<-emp_lengthc5



################                ################                ################                ################


#Column Removal

#NA filled columns

df<-subset(df ,select=-c(mths_since_last_delinq,mths_since_last_record,mths_since_last_major_derog,mths_since_last_major_derog,mths_since_recent_bc_dlq,
                         mths_since_recent_inq,mths_since_recent_revol_delinq,revol_util,mths_since_rcnt_il,il_util,all_util,bc_open_to_buy,bc_util,mo_sin_old_il_acct,mths_since_recent_bc,percent_bc_gt_75,last_credit_pull_d))


# 0 filled columns
df<-subset(df,select=-c(collections_12_mths_ex_med,num_tl_90g_dpd_24m))



#Unnecessary categoricals

df<-subset(df,select=-c(emp_title,earliest_cr_line,sub_grade,addr_state,verification_status))

################                ################                ################                ################

#Train and test set split 

trainindex <- sample(1:nrow(df), 0.7 * nrow(df))

train.set <- df[trainindex,]
test.set  <- df[-trainindex,]

################                ################                ################                ################


#Logistic regression model

glm.lr<-glm(train.set$loan_status ~.,data=train.set,family = "binomial")

summary(glm.lr)


#Prediction and accuracy

glm.pred<-predict(glm.lr,newdata=test.set,type='response')
glm.pred<-ifelse(glm.pred>0.5,1,0)

misclasserr<-mean(glm.pred ==test.set$loan_status)

print(paste('Accuracy',misclasserr))

#confusion matrix

confusion.lr<-table(actual = test.set$loan_status,predicted = glm.pred)
sum(diag(confusion.lr))/nrow(test.set)

#auc curve
rocp<-predict(glm.lr,newdata=test.set,type="response")
pr<-prediction(rocp,test.set$loan_status)


perf <- performance(pr, measure = "tpr", x.measure = "fpr")

plot(perf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

################                ################                ################                ################

#Random Forest Model

rf<- randomForest(train.set$loan_status~., data = train.set,ntree = 100)
varImpPlot(rf, sort = T, n.var = 15, main="Top 15")


#prediction

glm.pred.rf<-predict(rf,newdata=test.set,type='response')

confusion.rf<-table(actual = test.set$loan_status,predicted = glm.pred.rf)

sum(diag(confusion.rf))/nrow(test.set)


rf.rocCurve <- roc(train.set$loan_status,rf$votes[,2])

plot(rf.rocCurve)

auc(rf.rocCurve) 

################                ################                ################                ################

###c50 tree
model<-C5.0(train.set$loan_status~., data = train.set)

results<-predict(object = model, newdata = test.set, type="class")

conf.c50<-table(results, newdata=test.set$loan_status)

sum(diag(conf.c50))/nrow(test.set)








#####     NEXT STEPS

# Throw in correlations and descriptive data stuff

# Try again to plot the log reg curves

# Check for NAs kill them all

# Tune Logreg model so that no error for 0 and 1 probabilities

# Run the models on larger portions of datasets

# Verify and report on accuracies

# Start write-up

    ### EXTRA

    # Toss currents into models 
    # Fix if possible 

    # Match Rows with 2018 data 

    # Run cross check "currents" with 2018 data

# Write u[]