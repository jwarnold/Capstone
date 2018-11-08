#Packages
#install.packages("randomForest")
#install.packages("caret")
#install.packages("party")
#install.packages("ISLR")
#install.packages("corrplot")
#install.packages("ROCR")

#library(randomForest)
#library(caret)
#library(party)
#library(ISLR)
#library(corrplot)
#library(ROCR)


#Data load

df<-data.frame(read.csv("C:/Users/jatosan/Desktop/CAPSTONE/semifinal/secondtry/2500_column_gone_2016Q1.csv"))

#Removing "current" rows
df<-subset(df,!(df$loan_status=="Current"))

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



#Counting NAs in the following columns

sum(is.na(df$emp_title))
sum(is.na(df$mths_since_last_delinq)) #646
sum(is.na(df$mths_since_last_record)) #1134
sum(is.na(df$mths_since_last_major_derog)) #992 
sum(is.na(df$mths_since_recent_bc_dlq)) #1051
sum(is.na(df$mths_since_recent_inq)) #110
sum(is.na(df$mths_since_recent_revol_delinq))#894
sum(is.na(df$il_util)) #194



#Removing the columns with a sizable proportion of the rows are NAs

df<-subset(df ,select=-c(mths_since_last_delinq,mths_since_last_record,mths_since_last_major_derog,mths_since_last_major_derog,mths_since_recent_bc_dlq,
              mths_since_recent_inq,mths_since_recent_revol_delinq,revol_util,mths_since_rcnt_il,il_util,all_util,bc_open_to_buy,bc_util,mo_sin_old_il_acct,mths_since_recent_bc,percent_bc_gt_75,last_credit_pull_d))


#Removing columns full of 0s

length(which(df$collections_12_mths_ex_med==0)) #1367
length(which(df$num_tl_90g_dpd_24m==0)) #1325


df<-subset(df,select=-c(collections_12_mths_ex_med,num_tl_90g_dpd_24m))

df<-subset(df,select=-c(emp_title,earliest_cr_line,grade,sub_grade,purpose,addr_state,verification_status))
           



#Logistic Regression Modelling

glm.lr<-glm(df$loan_status ~.,data=df,family = binomial,maxit=100)

vim<-varImp(glm.lr)
impr<-which((varImp(glm.lr)>0.5))


t1<-apply(vim,2,function(x) x[order(-x)])
t1


#Training and test data indexing

trainindex <- sample(1:nrow(df), 0.7 * nrow(df))

train.set <- df[trainindex,]
test.set  <- df[-trainindex,]


#Prediction and accuracy

glm.pred<-predict(glm.lr,newdata=test.set,type='response')
glm.pred<-ifelse(glm.pred>0.5,1,0)

misclasserr<-mean(glm.pred ==testset$loan_status)

print(paste('Accuracy',1-misclasserr))

glm<-glm.pred
#curve(predict(glm.lr,data.frame(default=x),type="resp"),add=TRUE)
  

#ROC curve, report on accuracies of models.


rocp<-predict(glm.lr,newdata=test.set,type="response")
pr<-prediction(rocp,test.set$loan_status)


perf <- performance(pr, measure = "tpr", x.measure = "fpr")

plot(perf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc





















