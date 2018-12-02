
#Packages

install.packages("reshape", repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com")) #Correlation heatmap
install.packages("ggplot2", repos = c("http://rstudio.org/_packages","http://cran.rstudio.com")) #Correlation heatmatp
install.packages("randomForest", repos = c("http://rstudio.org/_packages","http://cran.rstudio.com")) #Random Forest
install.packages("caret", repos = c("http://rstudio.org/_packages","http://cran.rstudio.com")) #Classification And REgression Training
install.packages("party", repos = c("http://rstudio.org/_packages","http://cran.rstudio.com")) #Modelling
install.packages("ISLR", repos = c("http://rstudio.org/_packages","http://cran.rstudio.com"))
install.packages("corrplot", repos = c("http://rstudio.org/_packages","http://cran.rstudio.com"))
install.packages("ROCR", repos = c("http://rstudio.org/_packages","http://cran.rstudio.com"))
install.packages("tree", repos = c("http://rstudio.org/_packages","http://cran.rstudio.com"))
install.packages("RWeka", repos = c("http://rstudio.org/_packages","http://cran.rstudio.com")) #C4.5 tree
install.packages("e1071", repos = c("http://rstudio.org/_packages","http://cran.rstudio.com")) #svmachine
install.packages("rpart", repos = c("http://rstudio.org/_packages","http://cran.rstudio.com"))
install.packages("pROC", repos = c("http://rstudio.org/_packages","http://cran.rstudio.com")) #ROC curves
install.packages("C50", repos = c("http://rstudio.org/_packages","http://cran.rstudio.com"))# C50 tree
install.packages("printr") #C50 tree


library(reshape)
library(ggplot2)
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

#Load

#Full Dataset
df<-data.frame(read.csv("C:/Users/jatosan/Desktop/CAPSTONE/Final stuff/All_column_gone_2016Q1.csv"))

#Numeric Dataset
dfn<-data.frame(read.csv("C:/Users/jatosan/Desktop/CAPSTONE/Final stuff/numeric_2500.csv"))



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

#Descriptive Stats

hist(df$int_rate, col = "cornflowerblue", main = "Interest Rate Distribution", xlab = "Interest rate")

hist(df$loan_amnt, col ="chartreuse4", main = "Loan Amount Distribution", xlab = "Loan Amount")

hist(df$emp_length, col ="darkcyan", main = "Employment Length Distribution", xlab = "Employment Length")


################                ################                ################                ################


# Correlations and Descriptive Statistics on Numeric Columns

int_rateclean<-as.numeric(gsub("%","",dfn$int_rate))
dfn$int_rate<-int_rateclean

revol_utilcleann<-as.numeric(gsub("%","",dfn$revol_util))
dfn$revol_util<-revol_utilcleann

emp_lengthc1<-gsub(" years | year|s|+","",dfn$emp_length)
emp_lengthc2<-sub("10+","10",emp_lengthc1,fixed = TRUE)
emp_lengthc3<-sub("< 1","0",emp_lengthc2,fixed = TRUE)
emp_lengthc4<-sub("n/a","0",emp_lengthc3,fixed = TRUE)
emp_lengthc5<-as.numeric(emp_lengthc4)

dfn$emp_length<-emp_lengthc5

dfn[is.na(dfn)]<-0

cormat<-cor(dfn)
cormat[cormat<0.01]<- 0 
corrs<-round(cormat,2)

corrplot.mixed(cor(dfn), order="hclust", tl.col="black") #Not useful, too many columns

which(corrs>0.85&corrs!=1.00,arr.ind = TRUE)

#Check for low variance
vardf<-as.data.frame(format(apply(dfn,2,var),scientific = FALSE))



################                ################                ################                ################


#Column Removal


#NA filled columns

df<-subset(df ,select=-c(mths_since_last_delinq,mths_since_last_record,mths_since_last_major_derog,mths_since_last_major_derog,mths_since_recent_bc_dlq,
                         mths_since_recent_inq,mths_since_recent_revol_delinq,revol_util,mths_since_rcnt_il,il_util,all_util,bc_open_to_buy,bc_util,mo_sin_old_il_acct,mths_since_recent_bc,percent_bc_gt_75,last_credit_pull_d))

#low variance

df<-subset(df, select=-c(collections_12_mths_ex_med,open_il_12m,open_rv_24m,num_tl_90g_dpd_24m))

# 0 filled columns
df<-subset(df,select=-c(out_prncp))



#Unnecessary categoricals

df<-subset(df,select=-c(emp_title,earliest_cr_line,sub_grade,addr_state,verification_status))


#Check again for NAs
which(is.na(df),arr.ind = TRUE)
NArows<-as.data.frame(which(is.na(df),arr.ind = TRUE))
df.NArows<-NArows$row
df<-df[-c(df.NArows),]

threeclass<-(which(df$loan_status==3,arr.ind=TRUE))
threeclass<-as.data.frame(threeclass)
df.threeclassrow<-threeclass$threeclass
df<-df[-c(df.threeclassrow),]

df$loan_status<-factor(df$loan_status)

################                ################                ################                ################


#Train and test set split 

trainindex <- sample(1:nrow(df), 0.7 * nrow(df))

train.set <- df[trainindex,]
test.set  <- df[-trainindex,]


################                ################                ################                ################


                         ################            Modelling            ################  


################                ################                ################                ################



#Logistic regression model


glm.lr<-glm(train.set$loan_status ~.,data=train.set,family = "binomial")

summary(glm.lr)


#Prediction and accuracy

glm.pred<-predict(glm.lr,newdata=test.set,type='response')
glm.pred<-ifelse(glm.pred>0.5,1,0)



#confusion matrix

confusion.lr<-table(actual = test.set$loan_status,predicted = glm.pred)
sum(diag(confusion.lr))/nrow(test.set)

#auc curve
rocp<-predict(glm.lr,newdata=test.set,type="response")
pr<-prediction(rocp,test.set$loan_status)


perf <- performance(pr, measure = "tpr", x.measure = "fpr")

plot(perf,col="blue",main = "Logistic Regression ROC curve")
abline(a=0,b=1,col="red")
legend("topleft", "AUC = 0.7466")


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


################                ################                ################                ################

#Random Forest Model

rf<- randomForest(train.set$loan_status~., data = train.set,ntree = 100)
varim<-varImp(rf)
varim.t<-apply(varim,2,function(x) x[order(-x)])
varim.t
varImpPlot(rf, sort = T, n.var = 15, main="Top 15")



#prediction

glm.pred.rf<-predict(rf,newdata=test.set,type='response')

confusion.rf<-table(actual = test.set$loan_status,predicted = glm.pred.rf)

sum(diag(confusion.rf))/nrow(test.set)


rf.rocCurve <- roc(train.set$loan_status,rf$votes[,2])

plot(rf.rocCurve,col="blue", main="Random Forest ROC curve")
legend("topleft", "AUC = 0.7362")

auc(rf.rocCurve) 

plot(perf,col="blue",main = "Logistic Regression ROC curve")
abline(a=0,b=1,col="red")
legend("topleft", "AUC = 0.7466")




################                ################                ################                ################

###c50 tree
model<-C5.0(train.set$loan_status~., data = train.set)

results<-predict(object = model, newdata = test.set, type="class")

conf.c50<-table(results, newdata=test.set$loan_status)

sum(diag(conf.c50))/nrow(test.set)

summary(model)


################                ################         END        ################                ################
